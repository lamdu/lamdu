module Lamdu.DataFile
    ( getDataFilePath, getLamduDir
    ) where

import           Control.Lens.Operators
import           Paths_Lamdu (getDataFileName)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

getDataFilePath :: FilePath -> FilePath -> IO FilePath
getDataFilePath startDir fileName =
    do
        exists <- Directory.doesFileExist customPath
        if exists then return customPath else getDataFileName fileName
    where
        customPath = startDir </> fileName
