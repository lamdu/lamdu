module Lamdu.Paths
    ( getDataFileName
    , getLamduDir
    , readDataFile
    ) where

import           Control.Monad.Except (runExceptT, throwError)
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.Environment.Executable (splitExecutablePath)
import           System.FilePath ((</>))

import           Lamdu.Prelude

-- | Data-dir as in the .cabal file
dataDir :: FilePath
dataDir = "data"

searchPaths :: FilePath -> [IO FilePath]
searchPaths path =
    [ Directory.getCurrentDirectory <&> (</> dataDir </> path)
    , splitExecutablePath <&> fst <&> (</> dataDir </> path)
    , Paths_Lamdu.getDataFileName path
    ]

getDataFileName :: FilePath -> IO FilePath
getDataFileName fileName =
    flip traverse_ (searchPaths fileName)
        (\mkSearchPath ->
        do
            path <- lift mkSearchPath
            exists <- Directory.doesFileExist path & lift
            when exists $ throwError path -- Early exit, not an error
        )
    & runExceptT
    >>= either pure (const (fail ("Cannot find " ++ show fileName)))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

readDataFile :: FilePath -> IO String
readDataFile path = getDataFileName path >>= readFile
