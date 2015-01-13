{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Graphics.UI.Bottle.Widget
  ( module Graphics.UI.Bottle.WidgetId
  , Widget(..)
  , wMaybeEnter, wEventMap, wView
  , wFocalArea , wIsFocused, wAnimLayers
  , wAnimFrame, wSize, wWidth, wHeight, wEvents
  , empty
  , Enter, R, Size
  , EnterResult(..), enterResultEvent, enterResultRect
  , EventHandlers
  , EventResult(..), eventResultFromCursor
  , keysEventMap
  , keysEventMapMovesCursor
  , eAnimIdMapping, eCursor
  , takesFocus, doesntTakeFocus
  , backgroundColor, tint
  , fromView
  , addInnerFrame
  , strongerEvents, weakerEvents
  , translate, scale, pad, assymetricPad, padToSizeAlign
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Direction (Direction)
import qualified Graphics.UI.Bottle.Direction as Direction
import           Graphics.UI.Bottle.EventMap (EventMap)
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey)
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.WidgetId (Id(..), augmentId, toAnimId, joinId, subId)

data EventResult = EventResult
  { _eCursor :: Monoid.Last Id
  , _eAnimIdMapping :: Monoid.Endo AnimId
  } deriving (Generic)
instance Monoid EventResult where
  mempty = def_mempty
  mappend = def_mappend

data EnterResult f = EnterResult
  { _enterResultRect :: Rect
  , _enterResultEvent :: f EventResult
  }

type Enter f = Direction -> EnterResult f
type EventHandlers f = EventMap (f EventResult)

data Widget f = Widget
  { _wIsFocused :: Bool
  , _wView :: View
  , _wMaybeEnter :: Maybe (Enter f) -- Nothing if we're not enterable
  , _wEventMap :: EventHandlers f
  , _wFocalArea :: Rect
  }

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Widget

empty :: Widget f
empty = Widget False View.empty Nothing mempty (Rect 0 0)

{-# INLINE wAnimFrame #-}
wAnimFrame :: Lens' (Widget f) Anim.Frame
wAnimFrame = wView . View.animFrame

{-# INLINE wAnimLayers #-}
wAnimLayers :: Lens.Traversal' (Widget f) Anim.Layer
wAnimLayers = wAnimFrame . Anim.layers

{-# INLINE wSize #-}
wSize :: Lens' (Widget f) Size
wSize = wView . View.size

{-# INLINE wWidth #-}
wWidth :: Lens' (Widget f) R
wWidth = wView . View.width

{-# INLINE wHeight #-}
wHeight :: Lens' (Widget f) R
wHeight = wView . View.height

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult
  { _eCursor = Monoid.Last $ Just cursor
  , _eAnimIdMapping = mempty
  }

wEvents :: Lens.Setter (Widget f) (Widget g) (f EventResult) (g EventResult)
wEvents =
  Lens.sets atEvents
  where
    atEvents f widget = widget
      { _wMaybeEnter =
           (Lens.mapped . Lens.mapped . enterResultEvent %~ f) $
           _wMaybeEnter widget
      , _wEventMap = f <$> _wEventMap widget
      }

fromView :: View -> Widget f
fromView view =
  Widget
    { _wIsFocused = False
    , _wFocalArea = Rect 0 (view ^. View.size)
    , _wView = view
    , _wEventMap = mempty
    , _wMaybeEnter = Nothing
    }

takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter w = w & wMaybeEnter .~ mEnter
  where
    mEnter =
      Just $
      EnterResult focalArea .
      fmap eventResultFromCursor <$> enter
    focalArea = w ^. wFocalArea

doesntTakeFocus :: Widget f -> Widget f
doesntTakeFocus = wMaybeEnter .~ Nothing

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents events = wEventMap %~ (events `mappend`)

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents events = wEventMap %~ (`mappend` events)

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId color =
  wView %~ View.backgroundColor animId layer color

addInnerFrame :: Int -> AnimId -> Draw.Color -> Vector2 R -> Widget f -> Widget f
addInnerFrame layer animId color frameWidth widget =
  widget & wAnimFrame %~ mappend emptyRectangle
  where
    emptyRectangle =
      Anim.emptyRectangle frameWidth (widget ^. wSize) animId
      & Anim.unitImages %~ Draw.tint color
      & Anim.layers +~ layer


tint :: Draw.Color -> Widget f -> Widget f
tint color = wAnimFrame . Anim.unitImages %~ Draw.tint color

keysEventMap ::
  Functor f => [ModKey] -> EventMap.Doc ->
  f () -> EventHandlers f
keysEventMap keys doc act =
  (fmap . const) mempty <$>
  EventMap.keyPresses keys doc act

keysEventMapMovesCursor ::
  Functor f => [ModKey] -> EventMap.Doc ->
  f Id -> EventHandlers f
keysEventMapMovesCursor keys doc act =
  fmap eventResultFromCursor <$>
  EventMap.keyPresses keys doc act

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
-- Should expose higher-level combinators instead?
translate :: Vector2 R -> Widget f -> Widget f
translate pos widget =
  widget
  & wMaybeEnter . Lens._Just . Lens.argument .
    Direction.coordinates . Rect.topLeft -~ pos
  & wMaybeEnter . Lens._Just . Lens.mapped .
    enterResultRect . Rect.topLeft +~ pos
  & wFocalArea . Rect.topLeft +~ pos
  & wAnimFrame %~ Anim.translate pos

scale :: Vector2 R -> Widget f -> Widget f
scale mult widget =
  widget
  & wView %~ View.scale mult
  & wFocalArea . Rect.topLeftAndSize *~ mult
  & wMaybeEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
  & wMaybeEnter . Lens._Just . Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult

-- Surround a widget with padding
pad :: Vector2 R -> Widget f -> Widget f
pad p = assymetricPad p p

assymetricPad :: Vector2 R -> Vector2 R -> Widget f -> Widget f
assymetricPad leftAndTop rightAndBottom widget =
  widget
  & wSize +~ leftAndTop + rightAndBottom
  & translate leftAndTop

padToSizeAlign :: Size -> Vector2 R -> Widget f -> Widget f
padToSizeAlign newSize alignment widget =
  widget
  & translate (sizeDiff * alignment)
  & wSize .~ newSize
  where
    sizeDiff = max <$> 0 <*> newSize - widget ^. wSize
