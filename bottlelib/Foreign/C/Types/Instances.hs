{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module Foreign.C.Types.Instances () where

import Foreign.C.Types (CDouble)
import Data.Aeson (ToJSON(..), FromJSON(..))

instance FromJSON CDouble where
  parseJSON = fmap (realToFrac :: Double -> CDouble) . parseJSON
instance ToJSON CDouble where
  toJSON = toJSON . (realToFrac :: CDouble -> Double)
