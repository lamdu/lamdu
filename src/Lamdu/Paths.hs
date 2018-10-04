module Lamdu.Paths
    ( getDataFileName
    , getLamduDir
    , readDataFile
    ) where

import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

-- | Data-dir as in the .cabal file
dataDir :: FilePath
dataDir = "data"

getDataFileName :: FilePath -> IO FilePath
getDataFileName fileName =
    do
        currentDir <- Directory.getCurrentDirectory
        let customPath = currentDir </> dataDir </> fileName
        exists <- Directory.doesFileExist customPath
        if exists then pure customPath else Paths_Lamdu.getDataFileName fileName

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

readDataFile :: FilePath -> IO String
readDataFile path = getDataFileName path >>= readFile
