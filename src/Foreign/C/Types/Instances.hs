{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# OPTIONS -fno-warn-orphans #-}
module Foreign.C.Types.Instances () where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Foreign.C.Types (CDouble(..))

import Prelude

deriving via Double instance FromJSON CDouble
deriving via Double instance ToJSON CDouble
