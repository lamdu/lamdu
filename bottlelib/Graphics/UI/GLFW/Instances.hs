{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module Graphics.UI.GLFW.Instances
  (
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
import Graphics.UI.GLFW (Key(..))

deriving instance Generic Key

instance ToJSON Key
instance FromJSON Key
