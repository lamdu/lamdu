module Numeric.Utils (encodeHex, word8Hex) where

import Data.Word (Word8)
import Numeric (showHex)
import qualified Data.ByteString as SBS

encodeHex :: SBS.ByteString -> String
encodeHex = concatMap word8Hex . SBS.unpack

word8Hex :: Word8 -> String
word8Hex n
    | n < 0x10 = '0' : showHex n ""
    | otherwise = showHex n ""
