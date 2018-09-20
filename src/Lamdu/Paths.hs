module Lamdu.Paths
    ( getDataFileName
    , getLamduDir
    ) where

import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

getDataFileName :: FilePath -> IO FilePath
getDataFileName = Paths.get Paths_Lamdu.getDataFileName

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")
