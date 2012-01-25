{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Graphics.UI.Bottle.Sized (Sized(..), atRequestedSize, atFromSize) where

import Graphics.UI.Bottle.SizeRange (Size, SizeRange)
import qualified Data.AtFieldTH as AtFieldTH

data Sized a = Sized
    { requestedSize :: SizeRange
    , fromSize :: Size -> a
    }
    deriving Functor

AtFieldTH.make ''Sized
