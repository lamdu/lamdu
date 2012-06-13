{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widget
  ( Widget(..), MEnter, R
  , EnterResult(..), atEnterResultEvent, atEnterResultRect
  , Id(..), atId, joinId, subId
  , SizeDependentWidgetData(..)
  , atSdwdMaybeEnter, atSdwdEventMap, atSdwdFrame, atSdwdFocalArea
  , EventResult(..), atEAnimIdMapping, atECursor
  , emptyEventResult, eventResultFromCursor
  , keysEventMap, keysEventMapMovesCursor
  , EventHandlers, atContent, atIsFocused
  , userIO, image, eventMap
  , takesFocus, atMkSizeDependentWidgetData, atSizeDependentWidgetData
  , atImageWithSize, atImage, atMaybeEnter, atEventMap, atEvents
  , backgroundColor, tint, liftView
  , strongerEvents, weakerEvents
  , translate, translateSizeDependentWidgetData
  , translateBy
  , scale, scaleSizeDependentWidgetData
  , align
  ) where

import Data.Binary (Binary)
import Data.List(isPrefixOf)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (AnimId, R)
import Graphics.UI.Bottle.Direction (Direction)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

newtype Id = Id {
  toAnimId :: AnimId
  }
  deriving (Eq, Ord, Show, Read, Binary, Monoid)

joinId :: Id -> AnimId -> Id
joinId (Id x) y = Id $ x ++ y

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path)
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing


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

data Widget f = Widget {
  isFocused :: Bool,
  content :: Sized (SizeDependentWidgetData f)
  }

AtFieldTH.make ''EnterResult
AtFieldTH.make ''Id
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
  atSizeDependentWidgetData chg
  where
    chg userIo = userIo {
      sdwdMaybeEnter =
        (fmap . fmap . atEnterResultEvent) func $
        sdwdMaybeEnter userIo,
      sdwdEventMap = fmap func $ sdwdEventMap userIo
      }

liftView :: Sized Anim.Frame -> Widget f
liftView view =
  Widget {
    isFocused = False,
    content = Sized.atFromSize buildSizeDependentWidgetData view
    }
  where
    buildSizeDependentWidgetData mkFrame size =
      SizeDependentWidgetData {
        sdwdFocalArea = Rect 0 size,
        sdwdFrame = mkFrame size,
        sdwdEventMap = mempty,
        sdwdMaybeEnter = Nothing
        }

atSizeDependentWidgetData :: (SizeDependentWidgetData f -> SizeDependentWidgetData g) -> Widget f -> Widget g
atSizeDependentWidgetData = atContent . fmap

atMkSizeDependentWidgetData :: ((Size -> SizeDependentWidgetData f) -> Size -> SizeDependentWidgetData f) -> Widget f -> Widget f
atMkSizeDependentWidgetData = atContent . Sized.atFromSize

atImageWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atImageWithSize f = atMkSizeDependentWidgetData g
  where
    g mkSizeDependentWidgetData size = atSdwdFrame (f size) (mkSizeDependentWidgetData size)

atImage :: (Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atImage = atSizeDependentWidgetData . atSdwdFrame

userIO :: Widget f -> Size -> SizeDependentWidgetData f
userIO = Sized.fromSize . content

image :: Widget f -> Size -> Anim.Frame
image = (fmap . fmap) sdwdFrame userIO

eventMap :: Widget f -> Size -> EventHandlers f
eventMap = (fmap . fmap) sdwdEventMap userIO

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter = atSizeDependentWidgetData f
  where
    f sdwd = (atSdwdMaybeEnter . const) mEnter sdwd
      where
        mEnter = Just $ fmap (EnterResult focalArea . fmap eventResultFromCursor) enter
        focalArea = sdwdFocalArea sdwd

atMaybeEnter :: (MEnter f -> MEnter f) -> Widget f -> Widget f
atMaybeEnter = atSizeDependentWidgetData . atSdwdMaybeEnter

atEventMap :: (EventHandlers f -> EventHandlers f) -> Widget f -> Widget f
atEventMap = atSizeDependentWidgetData . atSdwdEventMap

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = atEventMap . flip mappend

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId = atImageWithSize . Anim.backgroundColor animId layer

tint :: Draw.Color -> Widget f -> Widget f
tint = atImage . Anim.onImages . Draw.tint

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
translate = atSizeDependentWidgetData . translateSizeDependentWidgetData

-- Translate a multiple of size
translateBy :: (Vector2 R -> Vector2 R) -> Widget f -> Widget f
translateBy translationOfSize = (atContent . Sized.atFromSize) f
  where
    f fromSize size = translateSizeDependentWidgetData (translationOfSize size) $ fromSize size

scaleSizeDependentWidgetData :: Vector2 R -> SizeDependentWidgetData f -> SizeDependentWidgetData f
scaleSizeDependentWidgetData mult =
  (atSdwdFrame . Anim.scale) mult .
  (atSdwdFocalArea . Rect.atTopLeftAndSize) (* mult) .
  (atSdwdMaybeEnter . fmap)
    ((fmap . atEnterResultRect . Rect.atTopLeftAndSize) (*mult) .
     (argument . Direction.inRelativePos . Rect.atTopLeftAndSize) (/mult))

scale :: Vector2 R -> Widget f -> Widget f
scale mult =
  (atSizeDependentWidgetData . scaleSizeDependentWidgetData) mult .
  atContent
    (Sized.atRequestedSize (SizeRange.lift2 (*) mult) .
     (Sized.atFromSize . argument) (/ mult))

-- If widget's max size is smaller than given size, place widget in
-- portion of the extra space (0..1 ratio in each dimension):
align :: Vector2 R -> Widget f -> Widget f
align = atContent . Sized.align translateSizeDependentWidgetData
