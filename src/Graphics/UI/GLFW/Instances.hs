{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Graphics.UI.GLFW.Instances
    (
    ) where

import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (defaultOptions)
import Graphics.UI.GLFW (Key(..), ModifierKeys(..))

deriveJSON defaultOptions ''Key
deriveJSON defaultOptions ''ModifierKeys
