{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), MEnter, Direction,
  UserIO(..), EventResult(..), EventHandlers, atContent, atIsFocused,
  atUioMaybeEnter, atUioEventMap, atUioFrame, eventResultFromCursor,
  userIO, image, eventMap, enter,
  takesFocus, atUserIO,
  atImageWithSize, atImage, atMaybeEnter, atEventMap, atEvents,
  backgroundColor, liftView, removeExtraSize,
  strongerKeys, weakerKeys) where

import Control.Applicative (liftA2)
import Data.Monoid (Monoid(..))
import Data.Record.Label (getL)
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (Frame)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

type Direction = Maybe (Vector2 Int)

type MEnter f = Maybe (Direction -> f EventResult)

-- TODO: EventResult will also include an AnimIds mapping
type Cursor = Anim.AnimId

data EventResult = EventResult {
  eCursor :: Cursor,
  eAnimIdMapping :: Anim.AnimId -> Anim.AnimId
  }

eventResultFromCursor :: Cursor -> EventResult
eventResultFromCursor cursor = EventResult {
  eCursor = cursor,
  eAnimIdMapping = id
  }

type EventHandlers f = EventMap (f EventResult)

data UserIO f = UserIO {
  uioFrame :: Frame,
  uioMaybeEnter :: MEnter f, -- Nothing if we're not enterable
  uioEventMap :: EventHandlers f
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
      uioMaybeEnter = (fmap . fmap) func $ uioMaybeEnter userIo,
      uioEventMap = fmap func $ uioEventMap userIo
      }

liftView :: Sized Frame -> Widget f
liftView view =
  Widget {
    isFocused = False,
    content = fmap buildUserIO view
    }
  where
    buildUserIO frame =
      UserIO {
        uioFrame = frame,
        uioEventMap = mempty,
        uioMaybeEnter = Nothing
        }

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

atUserIO :: (UserIO f -> UserIO g) -> Widget f -> Widget g
atUserIO = atContent . fmap

removeExtraSize :: Widget f -> Widget f
removeExtraSize = atContent f
  where
    f sized = (Sized.atFromSize . argument) (liftA2 cap maxSize) sized
      where
        cap Nothing y = y
        cap (Just x) y = min x y
        maxSize = getL SizeRange.srMaxSize $ Sized.requestedSize sized

atImageWithSize :: (Size -> Frame -> Frame) -> Widget f -> Widget f
atImageWithSize f = atContent . Sized.atFromSize $ g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Frame -> Frame) -> Widget f -> Widget f
atImage = atUserIO . atUioFrame

userIO :: Widget f -> Size -> UserIO f
userIO = Sized.fromSize . content

image :: Widget f -> Size -> Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget f -> Size -> EventHandlers f
eventMap = (fmap . fmap) uioEventMap userIO

enter :: Widget f -> Size -> Maybe (Direction -> f EventResult)
enter = (fmap . fmap) uioMaybeEnter userIO

-- ^ If Widget already takes focus, it is untouched
-- TODO: Would be nicer as (Direction -> Cursor), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Cursor) -> Widget f -> Widget f
takesFocus = atMaybeEnter . const . Just . (fmap . fmap) eventResultFromCursor

atMaybeEnter :: (MEnter f -> MEnter f) -> Widget f -> Widget f
atMaybeEnter = atUserIO . atUioMaybeEnter

atEventMap :: (EventHandlers f -> EventHandlers f) -> Widget f -> Widget f
atEventMap = atUserIO . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerKeys :: EventHandlers f -> Widget f -> Widget f
strongerKeys = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerKeys :: EventHandlers f -> Widget f -> Widget f
weakerKeys = atEventMap . flip mappend

backgroundColor :: Anim.AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor animId = atImageWithSize . Anim.backgroundColor animId 10
