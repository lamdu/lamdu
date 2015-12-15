{-# OPTIONS -fno-warn-orphans #-}
module Graphics.UI.GLFW.Instances
    (
    ) where

import qualified Data.Aeson.Types as Aeson
import           Graphics.UI.GLFW (Key(..), ModifierKeys(..))

instance Aeson.ToJSON Key where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Key

instance Aeson.ToJSON ModifierKeys where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON ModifierKeys
