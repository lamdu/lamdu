module Data.ByteString.Extended
    ( module SBS
    , lazify, strictify
    ) where

import           Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS

import           Prelude

strictify :: LBS.ByteString -> ByteString
strictify = SBS.concat . LBS.toChunks

lazify :: ByteString -> LBS.ByteString
lazify = LBS.fromChunks . pure
