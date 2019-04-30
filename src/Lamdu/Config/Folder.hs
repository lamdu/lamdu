module Lamdu.Config.Folder
    ( Selection, getFiles, getNames
    , themes
    ) where

import qualified Data.Text as Text
import qualified Lamdu.Paths as Paths
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

type Selection = Text

getFiles :: FilePath -> IO [FilePath]
getFiles folder =
    do
        themesDir <-
            Paths.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> folder)
        Directory.getDirectoryContents themesDir
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (themesDir </>)

getNames :: FilePath -> IO [Selection]
getNames folder = getFiles folder <&> map (Text.pack . FilePath.takeFileName . FilePath.dropExtension)

themes :: FilePath
themes = "themes"