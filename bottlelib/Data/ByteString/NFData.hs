{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 706
{-# OPTIONS -fno-warn-orphans #-} -- NFData ByteString
module Data.ByteString.NFData () where

import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as SBS

instance NFData SBS.ByteString where

#else

module Data.ByteString.NFData () where

#endif
