{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances,
             MultiParamTypeClasses, TupleSections #-}
module Graphics.UI.GLFWWidgets.Widget (Widget(..), image, eventMap, atImageWithSize, atHasFocus, atMaybeEventMap, liftView) where

import Control.Arrow (first, second)
import Control.Newtype
import Control.Newtype.TH
import Graphics.UI.GLFWWidgets.EventMap (EventMap)
import Graphics.UI.GLFWWidgets.SizeRange (Size)
import Graphics.UI.GLFWWidgets.Sized (Sized)
import Graphics.DrawingCombinators.Utils(Image)
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

atHasFocus :: (Bool -> Bool) -> Widget a -> Widget a
atHasFocus = over Widget . argument

atMaybeEventMap :: (Maybe (EventMap a) -> Maybe (EventMap b)) -> Widget a -> Widget b
atMaybeEventMap = over Widget . fmap . fmap . second

atFromSize :: ((Size -> UserIO a) -> Size -> UserIO b) -> Widget a -> Widget b
atFromSize = over Widget . fmap . Sized.atFromSize

atImageWithSize :: (Size -> Image -> Image) -> Widget a -> Widget a
atImageWithSize f = atFromSize g
  where
    g mkUserIO size = first (f size) (mkUserIO size)

image :: Widget k -> HasFocus -> Size -> Image
image = fmap (fmap fst . Sized.fromSize) . unpack

eventMap :: Widget k -> HasFocus -> Size -> Maybe (EventMap k)
eventMap = fmap (fmap snd . Sized.fromSize) . unpack
