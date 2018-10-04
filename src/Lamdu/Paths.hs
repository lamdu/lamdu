module Lamdu.Paths
    ( getDataFileName
    , getLamduDir
    ) where

import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

getDataFileName :: FilePath -> IO FilePath
getDataFileName fileName =
    do
        currentDir <- Directory.getCurrentDirectory
        let customPath = currentDir </> fileName
        exists <- Directory.doesFileExist customPath
        if exists then pure customPath else Paths_Lamdu.getDataFileName fileName

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")
