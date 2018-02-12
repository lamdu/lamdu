module Paths.Utils
    ( get
    ) where

import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Prelude

get :: (FilePath -> IO FilePath) -> FilePath -> IO FilePath
get getDataFileName fileName =
    do
        currentDir <- Directory.getCurrentDirectory
        let customPath = currentDir </> fileName
        exists <- Directory.doesFileExist customPath
        if exists then pure customPath else getDataFileName fileName
