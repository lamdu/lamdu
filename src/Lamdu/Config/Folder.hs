module Lamdu.Config.Folder
    ( Selection, initial, getFiles, getNames
    ) where

import qualified Data.Text as Text
import qualified Lamdu.Paths as Paths
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

type Selection = Text

getFiles :: IO [FilePath]
getFiles =
    do
        themesDir <-
            Paths.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> "themes")
        Directory.getDirectoryContents themesDir
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (themesDir </>)

getNames :: IO [Selection]
getNames = getFiles <&> map (Text.pack . FilePath.takeFileName . FilePath.dropExtension)

initial :: Selection
initial = "default"
