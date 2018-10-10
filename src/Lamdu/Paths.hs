module Lamdu.Paths
    ( getDataFileName
    , getDataFileNameMaybe
    , getLamduDir
    , readDataFile
    ) where

import           Control.Monad.Except (runExceptT, throwError)
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.Environment.Executable (splitExecutablePath)
import           System.FilePath ((</>))
import qualified System.Info as SysInfo

import           Lamdu.Prelude

-- | Data-dir as in the .cabal file
dataDir :: FilePath
dataDir = "data"

searchPaths :: FilePath -> [IO FilePath]
searchPaths path =
    [ Directory.getCurrentDirectory <&> (</> dataDir </> path)
    , splitExecutablePath <&> fst <&> (</> relDataDir </> path)
    , Paths_Lamdu.getDataFileName path
    ]
    where
        relDataDir
            | SysInfo.os == "darwin" = "../Resources"
            | otherwise = dataDir

getDataFileNameMaybe :: FilePath -> IO (Maybe FilePath)
getDataFileNameMaybe fileName =
    traverse_
        (\mkSearchPath ->
        do
            path <- lift mkSearchPath
            exists <- Directory.doesFileExist path & lift
            when exists $ throwError path -- Early exit, not an error
        )
        (searchPaths fileName)
    & runExceptT
    <&> either Just (\() -> Nothing)

getDataFileName :: FilePath -> IO FilePath
getDataFileName fileName =
    getDataFileNameMaybe fileName
    <&> fromMaybe (fail ("Cannot find " ++ show fileName))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

readDataFile :: FilePath -> IO String
readDataFile path = getDataFileName path >>= readFile
