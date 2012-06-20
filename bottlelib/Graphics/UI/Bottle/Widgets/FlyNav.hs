{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FlyNav(make, State, initState) where

import Control.Applicative (Applicative(..), liftA2, (*>))
import Control.Arrow (second)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget

data Movement = Movement
  { mName :: String
  , _mModKey :: EventMap.ModKey
  , mDir :: Vector2 Widget.R
  }

AtFieldTH.make ''Movement

data ActiveState = ActiveState
  { _asPos :: Vector2 Widget.R
  , _asMovements :: [Movement]
  }

type State = Maybe ActiveState

modifier :: EventMap.ModState
modifier = EventMap.ctrl `mappend` EventMap.shift

modifierKeys :: [EventMap.Key]
modifierKeys =
  [ EventMap.KeyLeftCtrl
  , EventMap.KeyRightCtrl
  , EventMap.KeyLeftShift
  , EventMap.KeyRightShift
  ]

initState :: State
initState = Nothing

withEmptyResult :: Functor f => f () -> f Widget.EventResult
withEmptyResult = (fmap . const) Widget.emptyEventResult

mkTickHandler :: Functor f => f () -> Widget.EventHandlers f
mkTickHandler = EventMap.tickHandler . withEmptyResult

mkKeyMap
  :: Functor f => EventMap.IsPress -> EventMap.ModKey -> EventMap.Doc
  -> f () -> Widget.EventHandlers f
mkKeyMap isPress key doc =
  EventMap.keyEventMap
  (EventMap.KeyEvent isPress key) doc .
  withEmptyResult

speed :: Vector2 Anim.R
speed = 12

accel :: Vector2 Anim.R
accel = 1.02

targetSize :: Vector2 Anim.R
targetSize = Vector2 25 25

targetColor :: Draw.Color
targetColor = Draw.Color 0.9 0.9 0 0.7

highlightColor :: Draw.Color
highlightColor = Draw.Color 0.4 0.4 1 0.4

target :: AnimId -> Vector2 Anim.R -> Anim.Frame
target animId pos =
  Anim.onDepth (subtract 100) .
  Anim.translate pos .
  Anim.scale targetSize .
  Anim.onImages (Draw.tint targetColor) .
  Anim.simpleFrame animId .
  (fmap . const) () $
  Draw.circle

cap :: Vector2 Anim.R -> Vector2 Anim.R -> Vector2 Anim.R
cap size = liftA2 max 0 . liftA2 min size

highlightRect :: AnimId -> Rect -> Anim.Frame
highlightRect animId (Rect pos size) =
  Anim.translate pos . Anim.scale size .
  Anim.onDepth (subtract 50) .
  Anim.onImages (Draw.tint highlightColor) $
  Anim.unitSquare animId

make
  :: Applicative f => AnimId -> State -> (State -> f ()) -> Widget f -> Widget f
make _ Nothing setState =
  Widget.atSizeDependentWidgetData notActive
  where
    notActive sdwd =
      (Widget.atSdwdEventMap . flip mappend)
      (notActiveEvents (Rect.center (Widget.sdwdFocalArea sdwd))) sdwd
    notActiveEvents pos = addMovements pos [] setState
make animId (Just (ActiveState pos movements)) setState =
  (Widget.atImage . mappend) (target (animId ++ ["target"]) pos) .
  Widget.atMkSizeDependentWidgetData onSdwd
  where
    delta = sum $ map mDir movements
    onSdwd mkSdwd size =
      (Widget.atSdwdEventMap . const) (eventMap mEnteredChild size) .
      maybe id (highlight . Widget.enterResultRect) mEnteredChild $
      sdwd
      where
        highlight =
          Widget.atSdwdFrame . mappend .
          highlightRect (animId ++ ["highlight"])
        mEnteredChild = fmap ($ targetPos) $ Widget.sdwdMaybeEnter sdwd
        sdwd = mkSdwd size
    targetPos =
      Direction.RelativePos $ Rect (pos - targetSize/2) targetSize
    nextState size =
      ActiveState (cap (pos + delta*speed) size)
      ((map . atMDir) (* accel) movements)
    eventMap mEnteredChild size = mconcat $
      (mkTickHandler . setState . Just . nextState) size :
      addMovements pos movements setState :
      finishMove mEnteredChild :
      [ stopMovement name modKey lessMovements
      | (Movement name modKey _, lessMovements) <- zipped movements
      ]
    finishMove = mconcat
    -- TODO: This is buggy, need to be able to be informed that the
    -- key combo was released, regardless of which mod/key was
    -- released first:
      [ finishOn (EventMap.ModKey modifier key)
      | key <- modifierKeys
      ]
    stopMovement name modKey newMovements =
      mkKeyMap EventMap.Release modKey ("Stop FlyNav " ++ name) .
      setState . Just $ ActiveState pos newMovements
    finishOn modKey mEnteredChild =
      EventMap.keyEventMap (EventMap.KeyEvent EventMap.Release modKey)
        "Stop FlyNav" $
        setState Nothing *>
        -- TODO: Just cancel FlyNav in any case if the MaybeEnter is
        -- Nothing...
        maybe (pure Widget.emptyEventResult) Widget.enterResultEvent
          mEnteredChild

addMovements
  :: Functor f
  => Vector2 Widget.R
  -> [Movement]
  -> (Maybe ActiveState -> f ())
  -> Widget.EventHandlers f
addMovements = mconcat
  [ addMovement "Down"  EventMap.KeyDown  (Vector2   0    1)
  , addMovement "Up"    EventMap.KeyUp    (Vector2   0  (-1))
  , addMovement "Right" EventMap.KeyRight (Vector2   1    0)
  , addMovement "Left"  EventMap.KeyLeft  (Vector2 (-1)   0)
  ]

addMovement
  :: Functor f
  => [Char]
  -> EventMap.Key
  -> Vector2 Widget.R
  -> Vector2 Widget.R
  -> [Movement]
  -> (Maybe ActiveState -> f ())
  -> Widget.EventHandlers f
addMovement name key dir pos movements setState
  | name `elem` map mName movements = mempty
  | otherwise =
    mkKeyMap EventMap.Press modKey ("Start FlyNav " ++ name) .
    setState . Just $ ActiveState pos (Movement name modKey dir : movements)
    where
      modKey = EventMap.ModKey modifier key

zipped :: [a] -> [(a, [a])]
zipped [] = []
zipped (x:xs) = (x, xs) : (map . second) (x:) (zipped xs)
