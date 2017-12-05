{-# LANGUAGE NoImplicitPrelude #-}

module Data.Binary.Utils
    ( decodeS, encodeS
    ) where

import           Data.Binary (Binary(..))
import qualified Data.Binary as Binary
import           Data.ByteString.Utils (lazifyBS, strictifyBS)

import           Lamdu.Prelude

decodeS :: Binary a => ByteString -> a
decodeS = Binary.decode . lazifyBS

encodeS :: Binary a => a -> ByteString
encodeS = strictifyBS . Binary.encode
