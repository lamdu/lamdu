{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FlyNav
    ( make
    , Config(..)
    , State
    , initState
    ) where

import           Control.Applicative (Applicative(..), liftA2, (*>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple

import           Data.Monoid (Monoid(..), (<>))
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..), ctrlMods, shiftMods)
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.Widget (Widget, Size)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Graphics.UI.GLFW as GLFW

newtype Config = Config
  { configLayer :: Anim.Layer
  }

data Movement = Movement
  { _mName :: String
  , __mModKey :: ModKey
  , _mDir :: Vector2 Widget.R
  }
Lens.makeLenses ''Movement

data ActiveState = ActiveState
  { _asPos :: Vector2 Widget.R
  , _asMovements :: [Movement]
  }

type State = Maybe ActiveState

modKey :: GLFW.Key -> ModKey
modKey = ModKey (ctrlMods <> shiftMods)

finishKeys :: [ModKey]
finishKeys =
  [ ModKey shiftMods GLFW.Key'LeftControl
  , ModKey shiftMods GLFW.Key'RightControl
  , ModKey ctrlMods GLFW.Key'LeftShift
  , ModKey ctrlMods GLFW.Key'RightShift
  ]

modifierKeys :: [GLFW.Key]
modifierKeys =
  [ GLFW.Key'LeftControl
  , GLFW.Key'RightControl
  , GLFW.Key'LeftShift
  , GLFW.Key'RightShift
  ]

initState :: State
initState = Nothing

withEmptyResult :: Functor f => f () -> f Widget.EventResult
withEmptyResult = (fmap . const) mempty

mkTickHandler :: Functor f => f () -> Widget.EventHandlers f
mkTickHandler = EventMap.tickHandler . withEmptyResult

mkKeyMap
  :: Functor f => GLFW.KeyState -> ModKey -> EventMap.Doc
  -> f () -> Widget.EventHandlers f
mkKeyMap isPress key doc =
  EventMap.keyEventMap
  (EventMap.KeyEvent isPress key) doc .
  withEmptyResult

speed :: Vector2 Widget.R
speed = 8

accel :: Vector2 Widget.R
accel = 1.05

targetSize :: Size
targetSize = Vector2 25 25

targetColor :: DrawUtils.Color
targetColor = DrawUtils.Color 0.9 0.9 0 0.7

highlightColor :: DrawUtils.Color
highlightColor = DrawUtils.Color 0.4 0.4 1 0.4

target :: Config -> AnimId -> Vector2 Widget.R -> Anim.Frame
target config animId pos =
  DrawUtils.circle
  & Anim.simpleFrame animId
  & Anim.unitImages %~ DrawUtils.tint targetColor
  & Anim.scale targetSize
  & Anim.translate pos
  & Anim.layers +~ configLayer config

cap :: Size -> Vector2 Widget.R -> Vector2 Widget.R
cap size = liftA2 max 0 . liftA2 min size

highlightRect :: AnimId -> Rect -> Anim.Frame
highlightRect animId (Rect pos size) =
  Anim.unitSquare animId
  & Anim.unitImages %~ DrawUtils.tint highlightColor
  & Anim.layers -~ 50 -- TODO: 50?!
  & Anim.scale size
  & Anim.translate pos

addMovements
  :: Functor f
  => Vector2 Widget.R
  -> [Movement]
  -> (Maybe ActiveState -> f ())
  -> Widget.EventHandlers f
addMovements = mconcat
  [ addMovement "Down"  (keysDown  stdDirKeys) (Vector2   0    1)
  , addMovement "Up"    (keysUp    stdDirKeys) (Vector2   0  (-1))
  , addMovement "Right" (keysRight stdDirKeys) (Vector2   1    0)
  , addMovement "Left"  (keysLeft  stdDirKeys) (Vector2 (-1)   0)
  ]

addMovement
  :: Functor f
  => String
  -> [GLFW.Key]
  -> Vector2 Widget.R
  -> Vector2 Widget.R
  -> [Movement]
  -> (Maybe ActiveState -> f ())
  -> Widget.EventHandlers f
addMovement name keys dir pos movements setState
  | name `elem` map (^. mName) movements = mempty
  | otherwise =
    mconcat
    [ mkKeyMap GLFW.KeyState'Pressed mk (EventMap.Doc ["Navigation", "FlyNav", name]) .
      setState . Just $ ActiveState pos (Movement name mk dir : movements)
    | key <- keys
    , let mk = modKey key
    ]

-- separate out a single element each time
zipped :: [a] -> [(a, [a])]
zipped [] = []
zipped (x:xs) =
  (x, xs) :
  (Lens.mapped . _2 %~ (x:)) (zipped xs)

focalCenter :: Lens' (Widget f) (Vector2 Widget.R)
focalCenter = Widget.focalArea . Rect.center

make
  :: Applicative f => Config -> AnimId -> State -> (State -> f ())
  -> Widget f -> Widget f
make _ _ Nothing setState w =
  w & Widget.eventMap <>~ addMovements (w ^. focalCenter) [] setState
make config animId (Just (ActiveState pos movements)) setState w =
  w
  & Widget.animFrame %~ mappend frame
  & Widget.eventMap .~ eventMap
  where
    delta = sum $ map (^. mDir) movements
    highlight =
      maybe mempty
      (highlightRect (animId ++ ["highlight"]) . (^. Widget.enterResultRect))
      mEnteredChild
    frame = target config (animId ++ ["target"]) pos `mappend` highlight
    mEnteredChild = fmap ($ targetPos) $ w ^. Widget.mEnter
    targetPos = Direction.Point pos
    nextState =
      ActiveState
      (cap (pos + delta*speed) (w ^. Widget.size)) $
      movements & Lens.mapped . mDir *~ accel
    eventMap = mconcat $
      (mkTickHandler . setState . Just) nextState :
      addMovements pos movements setState :
      finishMove :
      [ stopMovement name mk lessMovements
      | (Movement name mk _, lessMovements) <- zipped movements
      ]
    finishMove = mconcat $ map finishOn $ finishKeys ++ map modKey modifierKeys
    stopMovement name mk newMovements =
      mkKeyMap GLFW.KeyState'Released mk (EventMap.Doc ["Navigation", "Stop FlyNav", name]) .
      setState . Just $ ActiveState pos newMovements
    finishOn mk =
      EventMap.keyEventMap (EventMap.KeyEvent GLFW.KeyState'Released mk)
        (EventMap.Doc ["Navigation", "Stop FlyNav"]) $
        setState Nothing *>
        -- TODO: Just cancel FlyNav in any case if the MaybeEnter is
        -- Nothing...
        maybe (pure mempty) (^. Widget.enterResultEvent)
          mEnteredChild
