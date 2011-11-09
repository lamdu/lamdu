{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances,
             MultiParamTypeClasses #-}
module Widget (Widget(..), image, eventMap) where

import qualified Graphics.DrawingCombinators as Draw
import EventMap (EventMap)
import Sized (Sized, fromSize)
import Control.Newtype.TH
import Control.Newtype(unpack)
import SizeRange (Size)

type HasFocus = Bool

newtype Widget k = Widget (HasFocus -> Sized (Draw.Image (), Maybe (EventMap k)))
  deriving (Functor)
$(mkNewTypes [''Widget])

image :: Widget k -> HasFocus -> Size -> Draw.Image ()
image = fmap (fmap fst . fromSize) . unpack

eventMap :: Widget k -> HasFocus -> Size -> Maybe (EventMap k)
eventMap = fmap (fmap snd . fromSize) . unpack
