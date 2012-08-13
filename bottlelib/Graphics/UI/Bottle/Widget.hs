{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widget
  ( module Graphics.UI.Bottle.WidgetId
  , Widget(..), MEnter, R, Size
  , EnterResult(..), atEnterResultEvent, atEnterResultRect
  , EventHandlers
  , EventResult(..), emptyEventResult, eventResultFromCursor
  , keysEventMap, keysEventMapMovesCursor
  , atEAnimIdMapping, atECursor
  , atWMaybeEnter, atWEventMap, atWFrame, atWFocalArea
  , atWIsFocused, atWSize
  , atWFrameWithSize, atEvents
  , takesFocus, doesntTakeFocus
  , backgroundColor, tint, liftView
  , strongerEvents, weakerEvents
  , translate, translateBy, scale
  ) where

import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (AnimId, R, Size)
import Graphics.UI.Bottle.Direction (Direction)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.WidgetId (Id(..), atId, joinId, subId)
import qualified Control.Lens as Lens
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

data EventResult = EventResult {
  eCursor :: Maybe Id,
  eAnimIdMapping :: AnimId -> AnimId
  }

data EnterResult f = EnterResult {
  enterResultRect :: Rect,
  enterResultEvent :: f EventResult
  }

type MEnter f = Maybe (Direction -> EnterResult f)
type EventHandlers f = EventMap (f EventResult)

data Widget f = Widget
  { wIsFocused :: Bool
  , wSize :: Size
  , wFrame :: Anim.Frame
  , wMaybeEnter :: MEnter f -- Nothing if we're not enterable
  , wEventMap :: EventHandlers f
  , wFocalArea :: Rect
  }

AtFieldTH.make ''EnterResult
AtFieldTH.make ''EventResult
AtFieldTH.make ''Widget

emptyEventResult :: EventResult
emptyEventResult = EventResult {
  eCursor = Nothing,
  eAnimIdMapping = id
  }

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult {
  eCursor = Just cursor,
  eAnimIdMapping = id
  }

atEvents :: (f EventResult -> g EventResult) -> Widget f -> Widget g
atEvents func w = w {
  wMaybeEnter =
     (fmap . fmap . atEnterResultEvent) func $
     wMaybeEnter w,
  wEventMap = fmap func $ wEventMap w
  }

liftView :: Anim.Size -> Anim.Frame -> Widget f
liftView sz frame =
  Widget
    { wIsFocused = False
    , wSize = sz
    , wFocalArea = Rect 0 sz
    , wFrame = frame
    , wEventMap = mempty
    , wMaybeEnter = Nothing
    }

atWFrameWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atWFrameWithSize f w = atWFrame (f (wSize w)) w

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter w = (atWMaybeEnter . const) mEnter w
  where
    mEnter = Just $ fmap (EnterResult focalArea . fmap eventResultFromCursor) enter
    focalArea = wFocalArea w

doesntTakeFocus :: Widget f -> Widget f
doesntTakeFocus = (atWMaybeEnter . const) Nothing

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = atWEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = atWEventMap . flip mappend

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId = atWFrameWithSize . Anim.backgroundColor animId layer

tint :: Draw.Color -> Widget f -> Widget f
tint = atWFrame . Anim.onImages . Draw.tint

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
  (atWFrame . Anim.translate) pos .
  (atWFocalArea . Lens.over Rect.topLeft) (+pos) .
  (atWMaybeEnter . fmap)
    ((fmap . atEnterResultRect . Lens.over Rect.topLeft) (+pos) .
     (argument . Direction.inRelativePos . Lens.over Rect.topLeft) (subtract pos))

translateBy :: (Vector2 R -> Vector2 R) -> Widget f -> Widget f
translateBy mkPos w =
  (translate . mkPos . wSize) w w

scale :: Vector2 R -> Widget f -> Widget f
scale mult =
  (atWFrame . Anim.scale) mult .
  (atWFocalArea . Lens.over Rect.topLeftAndSize) (* mult) .
  (atWMaybeEnter . fmap)
    ((fmap . atEnterResultRect . Lens.over Rect.topLeftAndSize) (*mult) .
     (argument . Direction.inRelativePos . Lens.over Rect.topLeftAndSize) (/mult)) .
  atWSize (* mult)
