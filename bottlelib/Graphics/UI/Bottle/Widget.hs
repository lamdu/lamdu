{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widget
  ( module Graphics.UI.Bottle.WidgetId
  , Widget(..), MEnter, R, Size
  , EnterResult(..), enterResultEvent, enterResultRect
  , EventHandlers
  , EventResult(..), emptyEventResult, eventResultFromCursor
  , keysEventMap, keysEventMapMovesCursor
  , eAnimIdMapping, eCursor
  , wMaybeEnter, wEventMap, wFrame, wFocalArea
  , wIsFocused, wSize
  , atWFrameWithSize, atEvents
  , takesFocus, doesntTakeFocus
  , backgroundColor, tint, liftView
  , strongerEvents, weakerEvents
  , translate, translateBy, scale, scaleDownContent
  ) where

import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (AnimId, R, Size)
import Graphics.UI.Bottle.Direction (Direction)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.WidgetId (Id(..), toAnimId, joinId, subId)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

data EventResult = EventResult {
  _eCursor :: Maybe Id,
  _eAnimIdMapping :: AnimId -> AnimId
  }

data EnterResult f = EnterResult {
  _enterResultRect :: Rect,
  _enterResultEvent :: f EventResult
  }

type MEnter f = Maybe (Direction -> EnterResult f)
type EventHandlers f = EventMap (f EventResult)

data Widget f = Widget
  { _wIsFocused :: Bool
  , _wSize :: Size
  , _wFrame :: Anim.Frame
  , _wMaybeEnter :: MEnter f -- Nothing if we're not enterable
  , _wEventMap :: EventHandlers f
  , _wFocalArea :: Rect
  }

LensTH.makeLenses ''EnterResult
LensTH.makeLenses ''EventResult
LensTH.makeLenses ''Widget

emptyEventResult :: EventResult
emptyEventResult = EventResult {
  _eCursor = Nothing,
  _eAnimIdMapping = id
  }

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult {
  _eCursor = Just cursor,
  _eAnimIdMapping = id
  }

atEvents :: (f EventResult -> g EventResult) -> Widget f -> Widget g
atEvents func w = w
  { _wMaybeEnter =
       Lens.over (Lens.mapped . Lens.mapped . enterResultEvent) func $
       _wMaybeEnter w
  , _wEventMap = fmap func $ _wEventMap w
  }

liftView :: Anim.Size -> Anim.Frame -> Widget f
liftView sz frame =
  Widget
    { _wIsFocused = False
    , _wSize = sz
    , _wFocalArea = Rect 0 sz
    , _wFrame = frame
    , _wEventMap = mempty
    , _wMaybeEnter = Nothing
    }

atWFrameWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atWFrameWithSize f w = Lens.over wFrame (f (Lens.view wSize w)) w

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter w = Lens.set wMaybeEnter mEnter w
  where
    mEnter = Just $ fmap (EnterResult focalArea . fmap eventResultFromCursor) enter
    focalArea = Lens.view wFocalArea w

doesntTakeFocus :: Widget f -> Widget f
doesntTakeFocus = Lens.set wMaybeEnter Nothing

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = Lens.over wEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = Lens.over wEventMap . flip mappend

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId = atWFrameWithSize . Anim.backgroundColor animId layer

tint :: Draw.Color -> Widget f -> Widget f
tint = Lens.over wFrame . Anim.onImages . Draw.tint

keysEventMap ::
  Functor f => [EventMap.ModKey] -> EventMap.Doc ->
  f () -> EventHandlers f
keysEventMap keys doc act =
  (fmap . fmap . const) emptyEventResult $
  EventMap.keyPresses keys doc act

keysEventMapMovesCursor ::
  Functor f => [EventMap.ModKey] -> EventMap.Doc ->
  f Id -> EventHandlers f
keysEventMapMovesCursor keys doc act =
  (fmap . fmap) eventResultFromCursor $
  EventMap.keyPresses keys doc act

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
translate :: Vector2 R -> Widget f -> Widget f
translate pos =
  (Lens.over wFrame . Anim.translate) pos .
  (Lens.over wFocalArea . Lens.over Rect.topLeft) (+pos) .
  (Lens.over (wMaybeEnter . Lens.mapped))
    ((Lens.over (Lens.mapped . enterResultRect . Rect.topLeft)) (+pos) .
     (argument . Direction.atCoordinates . Lens.over Rect.topLeft) (subtract pos))

translateBy :: (Vector2 R -> Vector2 R) -> Widget f -> Widget f
translateBy mkPos w =
  (translate . mkPos . Lens.view wSize) w w

scale :: Vector2 R -> Widget f -> Widget f
scale mult =
  (Lens.over wFrame . Anim.scale) mult .
  (Lens.over wFocalArea . Lens.over Rect.topLeftAndSize) (* mult) .
  (Lens.over (wMaybeEnter . Lens.mapped))
    ((Lens.over (Lens.mapped . enterResultRect . Rect.topLeftAndSize)) (*mult) .
     (argument . Direction.atCoordinates . Lens.over Rect.topLeftAndSize) (/mult)) .
  Lens.over wSize (* mult)

-- | Scale down a widget without affecting its exported size
scaleDownContent :: Vector2 R -> Vector2 R -> Widget f -> Widget f
scaleDownContent factor align w =
  (Lens.over wSize . const) (Lens.view wSize w) .
  translate (Lens.view wSize w * align * (1 - factor)) .
  scale factor $
  w
