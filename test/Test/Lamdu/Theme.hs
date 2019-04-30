module Test.Lamdu.Theme (load) where

import qualified Data.Aeson.Config as AesonConfig
import qualified Data.Text as Text
import qualified Lamdu.Config.Folder as ConfigFolder
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Paths as Paths
import           System.FilePath ((</>), takeDirectory)

import           Test.Lamdu.Prelude

load :: IO Theme
load =
    Paths.getDataFileName "config.json"
    <&> takeDirectory
    <&> (\x -> x </> "themes" </> Text.unpack ConfigFolder.initial ++ ".json")
    >>= AesonConfig.load
