{-# OPTIONS -fno-warn-orphans #-}
module Foreign.C.Types.Instances () where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Foreign.C.Types (CDouble)

instance NFData CDouble where rnf = (`seq` ())

instance FromJSON CDouble where
  parseJSON = fmap (realToFrac :: Double -> CDouble) . parseJSON
instance ToJSON CDouble where
  toJSON = toJSON . (realToFrac :: CDouble -> Double)
