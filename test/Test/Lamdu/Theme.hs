module Test.Lamdu.Theme (load) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Paths as Paths
import qualified Lamdu.Themes as Themes
import           System.FilePath ((</>), takeDirectory)

import           Lamdu.Prelude

load :: IO Theme
load =
    Paths.getDataFileName "config.json"
    <&> takeDirectory
    <&> (\x -> x </> "themes" </> Text.unpack Themes.initial ++ ".json")
    >>= LBS.readFile
    <&> Aeson.eitherDecode'
    >>= either (fail . mappend "Failed loading default theme: ") pure
