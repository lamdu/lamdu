{-# LANGUAGE TypeApplications #-}
module Test.Lamdu.Theme (load) where

import qualified Data.Aeson.Config as AesonConfig
import           Data.Proxy (Proxy(..))
import           Lamdu.Config.Folder (HasConfigFolder(..))
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Paths as Paths
import           System.FilePath ((</>), takeDirectory)

import           Test.Lamdu.Prelude

load :: IO Theme
load =
    Paths.getDataFileName "config.json"
    <&> takeDirectory
    <&> (\x -> x </> configFolder (Proxy @Theme) </> "default.json")
    >>= AesonConfig.load
