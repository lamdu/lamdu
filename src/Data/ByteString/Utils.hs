module Data.ByteString.Utils
    ( lazifyBS, strictifyBS
    ) where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS

import           Prelude

strictifyBS :: LBS.ByteString -> SBS.ByteString
strictifyBS = SBS.concat . LBS.toChunks

lazifyBS :: SBS.ByteString -> LBS.ByteString
lazifyBS = LBS.fromChunks . pure
