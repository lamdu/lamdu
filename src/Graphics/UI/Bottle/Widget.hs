{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), MEnter, Direction,
  UserIO(..), EventResult, EventHandlers, atContent, atIsFocused,
  atUioMaybeEnter, atUioEventMap, atUioFrame,
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
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

type Direction = Maybe (Vector2 Int)

type MEnter f = Maybe (Direction -> f EventResult)

-- TODO: EventResult will also include an AnimIds mapping
type Cursor = Anim.AnimId
type EventResult = Cursor
type EventHandlers f = EventMap (f EventResult)

data UserIO f = UserIO {
  uioFrame :: Frame,
  uioMaybeEnter :: MEnter f, -- Nothing if we're not enterable
  uioEventMap :: EventHandlers f
  }

atUioFrame :: (Frame -> Frame) -> UserIO f -> UserIO f
atUioMaybeEnter :: (MEnter f -> MEnter f) -> UserIO f -> UserIO f
atUioEventMap :: (EventHandlers f -> EventHandlers f) -> UserIO f -> UserIO f
atUioFrame      f u = u { uioFrame      = f . uioFrame      $ u }
atUioMaybeEnter f u = u { uioMaybeEnter = f . uioMaybeEnter $ u }
atUioEventMap   f u = u { uioEventMap   = f . uioEventMap   $ u }

data Widget f = Widget {
  wIsFocused :: Bool,
  wContent :: Sized (UserIO f)
  }

atEvents :: (f EventResult -> g EventResult) -> Widget f -> Widget g
atEvents func =
  atUserIO chg
  where
    chg userIo = userIo {
      uioMaybeEnter = (fmap . fmap) func $ uioMaybeEnter userIo,
      uioEventMap = fmap func $ uioEventMap userIo
      }

atIsFocused :: (Bool -> Bool) -> Widget f -> Widget f
atIsFocused f w = w { wIsFocused = f (wIsFocused w) }

atContent ::
  (Sized (UserIO f) -> Sized (UserIO b)) ->
  Widget f -> Widget b
atContent f w = w { wContent = f (wContent w) }

liftView :: Sized Frame -> Widget f
liftView view =
  Widget {
    wIsFocused = False,
    wContent = fmap buildUserIO view
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
userIO = Sized.fromSize . wContent

image :: Widget f -> Size -> Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget f -> Size -> EventHandlers f
eventMap = (fmap . fmap) uioEventMap userIO

enter :: Widget f -> Size -> Maybe (Direction -> f EventResult)
enter = (fmap . fmap) uioMaybeEnter userIO

-- ^ If Widget flready takes focus, it is untouched
takesFocus :: (Direction -> f EventResult) -> Widget f -> Widget f
takesFocus = atMaybeEnter . const . Just

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
