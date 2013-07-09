module Data.Aeson.TH.CompatOptions (compatOptions) where

import Data.Aeson.TH (defaultOptions, Options(..), SumEncoding(..))

-- compatible with Aeson TH from <=0.6.1.*
compatOptions :: Options
compatOptions = defaultOptions { sumEncoding = ObjectWithSingleField }
