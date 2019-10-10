{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PolyKinds #-}

module Data.Binary.Extended
    ( module Data.Binary
    , decodeS, encodeS
    ) where

import Data.Binary
import Data.ByteString.Extended (ByteString, lazify, strictify)
import Data.Functor.Const

import Prelude

instance Binary a => Binary (Const a b)

decodeS :: Binary a => ByteString -> a
decodeS = decode . lazify

encodeS :: Binary a => a -> ByteString
encodeS = strictify . encode
