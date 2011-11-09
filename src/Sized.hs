{-# LANGUAGE DeriveFunctor #-}

module Sized (Sized(..), atFromSize) where

import SizeRange (Size, SizeRange)

data Sized a = Sized
    { requestedSize :: SizeRange
    , fromSize :: Size -> a
    }
    deriving Functor

atFromSize :: ((Size -> a) -> Size -> b) -> Sized a -> Sized b
atFromSize f (Sized reqSize fromSize) = Sized reqSize (f fromSize)
