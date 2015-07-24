{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
module Foreign.C.Types.Instances () where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Foreign.C.Types (CDouble)

#if !MIN_VERSION_deepseq(1,4,0)
import Control.DeepSeq (NFData(..))
instance NFData CDouble where rnf = (`seq` ())
#endif

instance FromJSON CDouble where
    parseJSON = fmap (realToFrac :: Double -> CDouble) . parseJSON
instance ToJSON CDouble where
    toJSON = toJSON . (realToFrac :: CDouble -> Double)
