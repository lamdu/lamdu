{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.Themes
    ( Selection, initial, getFiles, getNames
    ) where

import qualified Data.Text as Text
import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

type Selection = Text

getFiles :: IO [FilePath]
getFiles =
    do
        themesDir <-
            Paths.get Paths_Lamdu.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> "themes")
        Directory.getDirectoryContents themesDir
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (themesDir </>)

getNames :: IO [Selection]
getNames = getFiles <&> map (Text.pack . FilePath.takeFileName . FilePath.dropExtension)

initial :: Selection
initial = "default"
