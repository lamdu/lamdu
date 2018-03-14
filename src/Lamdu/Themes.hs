{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.Themes
    ( initial, getFiles
    ) where

import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

getFiles :: IO [FilePath]
getFiles =
    do
        themesDir <-
            Paths.get Paths_Lamdu.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> "themes")
        Directory.getDirectoryContents themesDir
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (themesDir </>)

initial :: Text
initial = "default"
