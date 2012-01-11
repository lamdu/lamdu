{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances,
             MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Widget (
  Widget(..),
  EventHandlers(..), atEhEnter, atEhEventMap,
  UserIO(..), atUioFrame, atUioMEventHandlers,
  image, eventMap, eventHandlers, takesFocus,
  atImageWithSize, atImage, atMaybeEventHandlers,
  atEnter, atEventMap,
  backgroundColor, liftView, removeExtraSize,
  strongerKeys, weakerKeys) where

import Control.Applicative (liftA2)
import Control.Newtype (unpack, over)
import Control.Newtype.TH (mkNewTypes)
import Data.Monoid (Monoid(..))
import Data.Record.Label (getL)
import Graphics.UI.Bottle.Animation (Frame)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

data EventHandlers k = EventHandlers {
  ehEnter :: k,
  ehEventMap :: EventMap k
  }
  deriving (Functor)
atEhEnter :: (a -> a) -> EventHandlers a -> EventHandlers a
atEhEnter f (EventHandlers enter eventMap) =
  EventHandlers (f enter) eventMap

atEhEventMap ::
  (EventMap a -> EventMap a) -> EventHandlers a -> EventHandlers a
atEhEventMap f (EventHandlers enter eventMap) =
  EventHandlers enter (f eventMap)

data UserIO k = UserIO {
  uioFrame :: Frame,
  uioMEventHandlers :: Maybe (EventHandlers k)
  }
  deriving (Functor)
atUioMEventHandlers ::
  (Maybe (EventHandlers a) -> Maybe (EventHandlers b)) ->
  UserIO a -> UserIO b
atUioMEventHandlers f (UserIO frame mEventHandlers) =
  UserIO frame (f mEventHandlers)

atUioFrame :: (Frame -> Frame) -> UserIO a -> UserIO a
atUioFrame f (UserIO frame mEventHandlers) =
  UserIO (f frame) mEventHandlers

newtype Widget k = Widget (Sized (UserIO k))
  deriving (Functor)
$(mkNewTypes [''Widget])

liftView :: Sized Frame -> Widget a
liftView = Widget . fmap (`UserIO` Nothing)

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

atSized :: (Sized (UserIO k) -> Sized (UserIO k')) -> Widget k -> Widget k'
atSized = over Widget

atMaybeEventHandlers ::
  (Maybe (EventHandlers a) -> Maybe (EventHandlers b)) ->
  Widget a -> Widget b
atMaybeEventHandlers = atSized . fmap . atUioMEventHandlers

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

image :: Widget k -> Size -> Frame
image = fmap uioFrame . Sized.fromSize . unpack

eventHandlers :: Widget k -> Size -> Maybe (EventHandlers k)
eventHandlers = fmap uioMEventHandlers . Sized.fromSize . unpack

eventMap :: Widget k -> Size -> Maybe (EventMap k)
eventMap = (fmap . fmap . fmap) ehEventMap eventHandlers

-- ^ If widget already takes focus, it is untouched
takesFocus :: a -> Widget a -> Widget a
takesFocus enter =
  atMaybeEventHandlers
  (Just .
   maybe (EventHandlers enter mempty) (atEhEnter . const $ enter))

atEnter :: (a -> a) -> Widget a -> Widget a
atEnter = atMaybeEventHandlers . fmap . atEhEnter

atEventMap :: (EventMap a -> EventMap a) -> Widget a -> Widget a
atEventMap = atMaybeEventHandlers . fmap . atEhEventMap

-- ^ If doesn't take focus, event map is ignored
strongerKeys :: EventMap a -> Widget a -> Widget a
strongerKeys = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerKeys :: EventMap a -> Widget a -> Widget a
weakerKeys = atEventMap . flip mappend

backgroundColor :: Anim.AnimId -> Draw.Color -> Widget a -> Widget a
backgroundColor animId = atImageWithSize . Anim.backgroundColor animId 10
