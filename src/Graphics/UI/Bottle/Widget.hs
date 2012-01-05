{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances,
             MultiParamTypeClasses, TupleSections #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), image, eventMap, takesFocus,
  atImageWithSize, atImage, atMaybeEventMap, liftView, removeExtraSize,
  strongerKeys, weakerKeys) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Control.Newtype (unpack, over)
import Control.Newtype.TH (mkNewTypes)
import Data.Record.Label (getL)
import Data.Monoid (Monoid(..))
import Graphics.UI.Bottle.Animation (Frame)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

type UserIO k = (Frame, Maybe (EventMap k))

newtype Widget k = Widget (Sized (UserIO k))
  deriving (Functor)
$(mkNewTypes [''Widget])

liftView :: Sized Frame -> Widget a
liftView = Widget . fmap (, Nothing)

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

atSized :: (Sized (UserIO k) -> Sized (UserIO k')) -> Widget k -> Widget k'
atSized = over Widget

atMaybeEventMap :: (Maybe (EventMap a) -> Maybe (EventMap b)) -> Widget a -> Widget b
atMaybeEventMap = atSized . fmap . second

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
    g mkUserIO size = first (f size) (mkUserIO size)

atImage :: (Frame -> Frame) -> Widget a -> Widget a
atImage = atImageWithSize . const

image :: Widget k -> Size -> Frame
image = fmap fst . Sized.fromSize . unpack

eventMap :: Widget k -> Size -> Maybe (EventMap k)
eventMap = fmap snd . Sized.fromSize . unpack

takesFocus :: Widget a -> Widget a
takesFocus = atMaybeEventMap $ maybe (Just mempty) Just

strongerKeys :: EventMap a -> Widget a -> Widget a
strongerKeys = atMaybeEventMap . mappend . Just

weakerKeys :: EventMap a -> Widget a -> Widget a
weakerKeys = atMaybeEventMap . flip mappend . Just
