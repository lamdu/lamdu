{-# LANGUAGE DeriveFunctor #-}

module Graphics.UI.Bottle.Sized (Sized(..), atFromSize) where

import Graphics.UI.Bottle.SizeRange (Size, SizeRange)

data Sized a = Sized
    { requestedSize :: SizeRange
    , fromSize :: Size -> a
    }
    deriving Functor

atFromSize :: ((Size -> a) -> Size -> b) -> Sized a -> Sized b
atFromSize f (Sized reqSize fromSz) = Sized reqSize (f fromSz)
