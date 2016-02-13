module Data.ByteString.Hex
    ( showHexByte
    , showHexBytes
    ) where

import qualified Data.ByteString as SBS
import           Data.Word (Word8)
import           Text.Printf (printf)

showHexByte :: Word8 -> String
showHexByte = printf "%02X"

showHexBytes :: SBS.ByteString -> String
showHexBytes = concatMap showHexByte . SBS.unpack
