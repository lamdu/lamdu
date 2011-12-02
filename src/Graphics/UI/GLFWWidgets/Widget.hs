{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances,
             MultiParamTypeClasses, TupleSections #-}
module Graphics.UI.GLFWWidgets.Widget (
  Widget(..), image, eventMap,
  atImageWithSize, atImage, atHasFocus, atMaybeEventMap, liftView, removeExtraSize) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Control.Newtype (unpack, over)
import Control.Newtype.TH (mkNewTypes)
import Data.Record.Label (getL)
import Graphics.DrawingCombinators.Utils(Image)
import Graphics.UI.GLFWWidgets.EventMap (EventMap)
import Graphics.UI.GLFWWidgets.SizeRange (Size)
import Graphics.UI.GLFWWidgets.Sized (Sized)
import qualified Graphics.UI.GLFWWidgets.SizeRange as SizeRange
import qualified Graphics.UI.GLFWWidgets.Sized as Sized

type HasFocus = Bool

type UserIO k = (Image, Maybe (EventMap k))

newtype Widget k = Widget (HasFocus -> Sized (UserIO k))
  deriving (Functor)
$(mkNewTypes [''Widget])

liftView :: Sized Image -> Widget a
liftView = Widget . const . fmap (, Nothing)

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

atHasFocus :: (Bool -> Bool) -> Widget a -> Widget a
atHasFocus = over Widget . argument

atSized :: (Sized (UserIO k) -> Sized (UserIO k')) -> Widget k -> Widget k'
atSized = over Widget . result

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

atImageWithSize :: (Size -> Image -> Image) -> Widget a -> Widget a
atImageWithSize f = atSized . Sized.atFromSize $ g
  where
    g mkUserIO size = first (f size) (mkUserIO size)

atImage :: (Image -> Image) -> Widget a -> Widget a
atImage = atImageWithSize . const

image :: Widget k -> HasFocus -> Size -> Image
image = fmap (fmap fst . Sized.fromSize) . unpack

eventMap :: Widget k -> HasFocus -> Size -> Maybe (EventMap k)
eventMap = fmap (fmap snd . Sized.fromSize) . unpack
