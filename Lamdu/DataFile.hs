module Lamdu.DataFile
    ( getDataFilePath, getLamduDir
    ) where

import           Control.Lens.Operators
import           Paths_Lamdu (getDataFileName)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

getDataFilePath :: FilePath -> IO FilePath
getDataFilePath fileName =
    do
        startDir <- Directory.getCurrentDirectory
        let customPath = startDir </> fileName
        exists <- Directory.doesFileExist customPath
        if exists then return customPath else getDataFileName fileName
