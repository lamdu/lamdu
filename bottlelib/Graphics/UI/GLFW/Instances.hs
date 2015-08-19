{-# OPTIONS -fno-warn-orphans #-}
module Graphics.UI.GLFW.Instances
    (
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Graphics.UI.GLFW (Key(..), ModifierKeys(..))

instance ToJSON Key
instance FromJSON Key

instance ToJSON ModifierKeys
instance FromJSON ModifierKeys
