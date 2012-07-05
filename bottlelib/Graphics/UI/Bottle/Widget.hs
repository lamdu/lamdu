{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widget
  ( module Graphics.UI.Bottle.WidgetId
  , Widget(..), MEnter, R, Size
  , EnterResult(..), atEnterResultEvent, atEnterResultRect
  , SizeDependentWidgetData(..)
  , EventHandlers
  , EventResult(..), emptyEventResult, eventResultFromCursor
  , keysEventMap, keysEventMapMovesCursor
  , atEAnimIdMapping, atECursor
  , atSdwdMaybeEnter, atSdwdEventMap, atSdwdFrame, atSdwdFocalArea
  , atWIsFocused, atWContent, atWSize
  , atWContentWithSize
  , atFrame, atFrameWithSize, atMaybeEnter, atEventMap, atEvents
  , takesFocus, doesntTakeFocus
  , backgroundColor, tint, liftView
  , strongerEvents, weakerEvents
  , translate, translateSizeDependentWidgetData
  , translateBy
  , scale, scaleSizeDependentWidgetData
  ) where

import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (AnimId, R, Size)
import Graphics.UI.Bottle.Direction (Direction)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.WidgetId (Id(..), atId, joinId, subId)
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

data SizeDependentWidgetData f = SizeDependentWidgetData {
  sdwdFrame :: Anim.Frame,
  sdwdMaybeEnter :: MEnter f, -- Nothing if we're not enterable
  sdwdEventMap :: EventHandlers f,
  sdwdFocalArea :: Rect
  }

data Widget f = Widget
  { wIsFocused :: Bool
  , wSize :: Size
  , wContent :: SizeDependentWidgetData f
  }

AtFieldTH.make ''EnterResult
AtFieldTH.make ''EventResult
AtFieldTH.make ''SizeDependentWidgetData
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
atEvents func =
  atWContent chg
  where
    chg userIo = userIo {
      sdwdMaybeEnter =
        (fmap . fmap . atEnterResultEvent) func $
        sdwdMaybeEnter userIo,
      sdwdEventMap = fmap func $ sdwdEventMap userIo
      }

liftView :: Anim.Size -> Anim.Frame -> Widget f
liftView sz frame =
  Widget
    { wIsFocused = False
    , wSize = sz
    , wContent = SizeDependentWidgetData
      { sdwdFocalArea = Rect 0 sz
      , sdwdFrame = frame
      , sdwdEventMap = mempty
      , sdwdMaybeEnter = Nothing
      }
    }

atWContentWithSize
  :: (Size -> SizeDependentWidgetData f -> SizeDependentWidgetData g)
  -> Widget f -> Widget g
atWContentWithSize f w = atWContent (f (wSize w)) w

atFrameWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atFrameWithSize f w = (atWContent . atSdwdFrame) (f (wSize w)) w

atFrame :: (Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atFrame = atWContent . atSdwdFrame

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter = atWContent f
  where
    f sdwd = (atSdwdMaybeEnter . const) mEnter sdwd
      where
        mEnter = Just $ fmap (EnterResult focalArea . fmap eventResultFromCursor) enter
        focalArea = sdwdFocalArea sdwd

doesntTakeFocus :: Widget f -> Widget f
doesntTakeFocus = (atMaybeEnter . const) Nothing

atMaybeEnter :: (MEnter f -> MEnter f) -> Widget f -> Widget f
atMaybeEnter = atWContent . atSdwdMaybeEnter

atEventMap :: (EventHandlers f -> EventHandlers f) -> Widget f -> Widget f
atEventMap = atWContent . atSdwdEventMap

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = atEventMap . flip mappend

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId = atFrameWithSize . Anim.backgroundColor animId layer

tint :: Draw.Color -> Widget f -> Widget f
tint = atFrame . Anim.onImages . Draw.tint

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

translateSizeDependentWidgetData :: Vector2 R -> SizeDependentWidgetData f -> SizeDependentWidgetData f
translateSizeDependentWidgetData pos =
  (atSdwdFrame . Anim.translate) pos .
  (atSdwdFocalArea . Rect.atRectTopLeft) (+pos) .
  (atSdwdMaybeEnter . fmap . fmap . atEnterResultRect . Rect.atRectTopLeft) (+pos) .
  (atSdwdMaybeEnter . fmap . argument . Direction.inRelativePos . Rect.atRectTopLeft) (subtract pos)

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
translate :: Vector2 R -> Widget f -> Widget f
translate = atWContent . translateSizeDependentWidgetData

translateBy :: (Vector2 R -> Vector2 R) -> Widget f -> Widget f
translateBy mkPos w =
  (atWContent . translateSizeDependentWidgetData . mkPos . wSize) w w

scaleSizeDependentWidgetData :: Vector2 R -> SizeDependentWidgetData f -> SizeDependentWidgetData f
scaleSizeDependentWidgetData mult =
  (atSdwdFrame . Anim.scale) mult .
  (atSdwdFocalArea . Rect.atTopLeftAndSize) (* mult) .
  (atSdwdMaybeEnter . fmap)
    ((fmap . atEnterResultRect . Rect.atTopLeftAndSize) (*mult) .
     (argument . Direction.inRelativePos . Rect.atTopLeftAndSize) (/mult))

scale :: Vector2 R -> Widget f -> Widget f
scale mult =
  (atWContent . scaleSizeDependentWidgetData) mult .
  (atWSize (* mult))
