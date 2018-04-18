module Numeric.Extended
    ( module Numeric
    , encodeHex, word8Hex
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as SBS
import           Data.Word (Word8)
import           Numeric

import           Prelude

encodeHex :: ByteString -> String
encodeHex = concatMap word8Hex . SBS.unpack

word8Hex :: Word8 -> String
word8Hex n
    | n < 0x10 = '0' : showHex n ""
    | otherwise = showHex n ""
