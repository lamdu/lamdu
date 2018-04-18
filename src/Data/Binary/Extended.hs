module Data.Binary.Extended
    ( module Data.Binary
    , decodeS, encodeS
    ) where

import           Data.Binary
import           Data.ByteString.Extended (ByteString, lazify, strictify)

import           Prelude

decodeS :: Binary a => ByteString -> a
decodeS = decode . lazify

encodeS :: Binary a => a -> ByteString
encodeS = strictify . encode
