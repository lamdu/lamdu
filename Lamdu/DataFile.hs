module Lamdu.DataFile
    ( getPath, getLamduDir
    ) where

import           Control.Lens.Operators
import           Paths_Lamdu (getDataFileName)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

getPath :: FilePath -> IO FilePath
getPath fileName =
    do
        startDir <- Directory.getCurrentDirectory
        let customPath = startDir </> fileName
        exists <- Directory.doesFileExist customPath
        if exists then return customPath else getDataFileName fileName
