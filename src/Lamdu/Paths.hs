module Lamdu.Paths
    ( getDataFileName
    ) where

import qualified Paths_Lamdu
import qualified Paths.Utils as Paths
import           System.FilePath (FilePath)

import           Lamdu.Prelude

getDataFileName :: FilePath -> IO FilePath
getDataFileName = Paths.get Paths_Lamdu.getDataFileName
