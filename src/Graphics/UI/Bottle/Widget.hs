{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances,
             MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), Direction,
  UserIO(..), atUioMaybeEnter, atUioEventMap, atUioFrame,
  userIO, image, eventMap, enter,
  takesFocus,
  atImageWithSize, atImage, atMaybeEnter, atEventMap,
  backgroundColor, liftView, removeExtraSize,
  strongerKeys, weakerKeys) where

import Control.Applicative (liftA2)
import Control.Newtype (unpack, over)
import Control.Newtype.TH (mkNewTypes)
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

type Direction = Vector2 Int

data UserIO k = UserIO {
  uioFrame :: Frame,
  uioMaybeEnter :: Maybe (Direction -> k), -- Nothing if we're not enterable
  uioEventMap :: EventMap k
  }
  deriving (Functor)

atUioFrame ::
  (Frame -> Frame) -> UserIO a -> UserIO a
atUioMaybeEnter ::
  (Maybe (Direction -> a) ->
   Maybe (Direction -> a)) -> UserIO a -> UserIO a
atUioEventMap ::
  (EventMap a -> EventMap a) -> UserIO a -> UserIO a
atUioFrame      f u = u { uioFrame      = f . uioFrame      $ u }
atUioMaybeEnter f u = u { uioMaybeEnter = f . uioMaybeEnter $ u }
atUioEventMap   f u = u { uioEventMap   = f . uioEventMap   $ u }

newtype Widget a = Widget (Sized (UserIO a))
  deriving (Functor)
$(mkNewTypes [''Widget])

liftView :: Sized Frame -> Widget a
liftView = Widget . fmap buildUserIO
  where
    buildUserIO frame =
      UserIO {
        uioFrame = frame,
        uioEventMap = mempty,
        uioMaybeEnter = Nothing
        }

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

atSized :: (Sized (UserIO a) -> Sized (UserIO b)) -> Widget a -> Widget b
atSized = over Widget

removeExtraSize :: Widget a -> Widget a
removeExtraSize = atSized f
  where
    f sized = (Sized.atFromSize . argument) (liftA2 cap maxSize) sized
      where
        cap Nothing y = y
        cap (Just x) y = min x y
        maxSize = getL SizeRange.srMaxSize $ Sized.requestedSize sized

atImageWithSize :: (Size -> Frame -> Frame) -> Widget a -> Widget a
atImageWithSize f = atSized . Sized.atFromSize $ g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Frame -> Frame) -> Widget a -> Widget a
atImage = atImageWithSize . const

userIO :: Widget a -> Size -> UserIO a
userIO = Sized.fromSize . unpack

image :: Widget a -> Size -> Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget a -> Size -> EventMap a
eventMap = (fmap . fmap) uioEventMap userIO

enter :: Widget a -> Size -> Maybe (Direction -> a)
enter = (fmap . fmap) uioMaybeEnter userIO

-- ^ If widget already takes focus, it is untouched
takesFocus :: (Direction -> a) -> Widget a -> Widget a
takesFocus = atMaybeEnter . const . Just

atMaybeEnter ::
  (Maybe (Direction -> a) ->
   Maybe (Direction -> a)) ->
  Widget a -> Widget a
atMaybeEnter = atSized . fmap . atUioMaybeEnter

atEventMap :: (EventMap a -> EventMap a) -> Widget a -> Widget a
atEventMap = atSized . fmap . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerKeys :: EventMap a -> Widget a -> Widget a
strongerKeys = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerKeys :: EventMap a -> Widget a -> Widget a
weakerKeys = atEventMap . flip mappend

backgroundColor :: Anim.AnimId -> Draw.Color -> Widget a -> Widget a
backgroundColor animId = atImageWithSize . Anim.backgroundColor animId 10
