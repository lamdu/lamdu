{-# OPTIONS -fno-warn-orphans #-} -- NFData ByteString
module Data.ByteString.NFData () where

import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as SBS

instance NFData SBS.ByteString where
