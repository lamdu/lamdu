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

newtype Widget k = Widget (Sized (Draw.Image (), Maybe (EventMap k)))
  deriving (Functor)
$(mkNewTypes [''Widget])

image :: Widget k -> Size -> Draw.Image ()
image = fmap fst . fromSize . unpack

eventMap :: Widget k -> Size -> Maybe (EventMap k)
eventMap = fmap snd . fromSize . unpack
