{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), MEnter, R,
  EnterResult(..), atEnterResultEvent, atEnterResultRect,
  Id(..), atId, joinId, subId,
  Direction(..), direction,
  UserIO(..), atUioMaybeEnter, atUioEventMap, atUioFrame, atUioFocalArea,
  EventResult(..), atEAnimIdMapping, atECursor,
  emptyEventResult, eventResultFromCursor,
  actionEventMap, actionEventMapMovesCursor,
  EventHandlers, atContent, atIsFocused,
  userIO, image, eventMap,
  takesFocus, atMkUserIO, atUserIO,
  atImageWithSize, atImage, atMaybeEnter, atEventMap, atEvents,
  backgroundColor, tint, liftView,
  strongerEvents, weakerEvents,
  translate, translateUserIO, align) where

import Data.Binary (Binary)
import Data.List(isPrefixOf)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (AnimId, R)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Sized as Sized

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | RelativePos Anim.Rect

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

-- cata:
direction :: r -> (Anim.Rect -> r) -> Direction -> r
direction outside _ Outside = outside
direction _ relativePos (RelativePos v) = relativePos v

inRelativePos :: (Anim.Rect -> Anim.Rect) -> Direction -> Direction
inRelativePos f = direction Outside (RelativePos . f)

data EnterResult f = EnterResult {
  enterResultRect :: Anim.Rect,
  enterResultEvent :: f EventResult
  }

type MEnter f = Maybe (Direction -> EnterResult f)

newtype Id = Id {
  cursorId :: AnimId
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

AtFieldTH.make ''EnterResult
AtFieldTH.make ''Id
AtFieldTH.make ''EventResult

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

type EventHandlers f = EventMap (f EventResult)

data UserIO f = UserIO {
  uioFrame :: Anim.Frame,
  uioMaybeEnter :: MEnter f, -- Nothing if we're not enterable
  uioEventMap :: EventHandlers f,
  uioFocalArea :: Anim.Rect
  }

AtFieldTH.make ''UserIO

data Widget f = Widget {
  isFocused :: Bool,
  content :: Sized (UserIO f)
  }

AtFieldTH.make ''Widget

atEvents :: (f EventResult -> g EventResult) -> Widget f -> Widget g
atEvents func =
  atUserIO chg
  where
    chg userIo = userIo {
      uioMaybeEnter =
        (fmap . fmap . atEnterResultEvent) func $
        uioMaybeEnter userIo,
      uioEventMap = fmap func $ uioEventMap userIo
      }

liftView :: Sized Anim.Frame -> Widget f
liftView view =
  Widget {
    isFocused = False,
    content = Sized.atFromSize buildUserIO view
    }
  where
    buildUserIO mkFrame size =
      UserIO {
        uioFocalArea = Anim.Rect 0 size,
        uioFrame = mkFrame size,
        uioEventMap = mempty,
        uioMaybeEnter = Nothing
        }

atUserIO :: (UserIO f -> UserIO g) -> Widget f -> Widget g
atUserIO = atContent . fmap

atMkUserIO :: ((Size -> UserIO f) -> Size -> UserIO f) -> Widget f -> Widget f
atMkUserIO = atContent . Sized.atFromSize

atImageWithSize :: (Size -> Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atImageWithSize f = atMkUserIO g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Anim.Frame -> Anim.Frame) -> Widget f -> Widget f
atImage = atUserIO . atUioFrame

userIO :: Widget f -> Size -> UserIO f
userIO = Sized.fromSize . content

image :: Widget f -> Size -> Anim.Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget f -> Size -> EventHandlers f
eventMap = (fmap . fmap) uioEventMap userIO

-- TODO: Would be nicer as (Direction -> Id), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enter = atUserIO f
  where
    f uio = (atUioMaybeEnter . const) mEnter uio
      where
        mEnter = Just $ fmap (EnterResult focalArea . fmap eventResultFromCursor) enter
        focalArea = uioFocalArea uio

atMaybeEnter :: (MEnter f -> MEnter f) -> Widget f -> Widget f
atMaybeEnter = atUserIO . atUioMaybeEnter

atEventMap :: (EventHandlers f -> EventHandlers f) -> Widget f -> Widget f
atEventMap = atUserIO . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = atEventMap . flip mappend

backgroundColor :: Id -> Draw.Color -> Widget f -> Widget f
backgroundColor (Id animId) = atImageWithSize . Anim.backgroundColor animId 10

tint :: Draw.Color -> Widget f -> Widget f
tint = atImage . Anim.onImages . Draw.tint

actionEventMap ::
  Functor f => [EventMap.EventType] -> EventMap.Doc ->
  f () -> EventHandlers f
actionEventMap keys doc act =
  (fmap . fmap . const) emptyEventResult $
  EventMap.fromEventTypes keys doc act

actionEventMapMovesCursor ::
  Functor f => [EventMap.EventType] -> EventMap.Doc ->
  f Id -> EventHandlers f
actionEventMapMovesCursor keys doc act =
  (fmap . fmap) eventResultFromCursor $
  EventMap.fromEventTypes keys doc act

translateUserIO :: Vector2 R -> UserIO f -> UserIO f
translateUserIO pos =
  (atUioFrame . Anim.translate) pos .
  (atUioFocalArea . Anim.atRectTopLeft) (+pos) .
  (atUioMaybeEnter . fmap . fmap . atEnterResultRect . Anim.atRectTopLeft) (+pos) .
  (atUioMaybeEnter . fmap . argument . inRelativePos . Anim.atRectTopLeft) (subtract pos)

translate :: Vector2 R -> Widget f -> Widget f
translate = atUserIO . translateUserIO

-- If widget's max size is smaller than given size, place widget in
-- portion of the extra space (0..1 ratio in each dimension):
align :: Vector2 R -> Widget f -> Widget f
align = atContent . Sized.align translateUserIO
