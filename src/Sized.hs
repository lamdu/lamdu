{-# LANGUAGE DeriveFunctor #-}

module Sized (Sized(..)) where

import SizeRange (Size, SizeRange)

data Sized a = Sized
    { requestedSize :: SizeRange
    , fromSize :: Size -> a
    }
    deriving Functor
