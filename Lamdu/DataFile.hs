module Lamdu.DataFile
    ( accessDataFile, getLamduDir
    ) where

import           Control.Lens.Operators
import           Paths_Lamdu (getDataFileName)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

accessDataFile :: FilePath -> (FilePath -> IO a) -> FilePath -> IO a
accessDataFile startDir accessor fileName =
    do
        exists <- Directory.doesFileExist customPath
        accessor =<< if exists then return customPath else getDataFileName fileName
    where
        customPath = startDir </> fileName
