module Lamdu.DataFile
    ( accessDataFile, getLamduDir
    ) where

import qualified Control.Exception as E
import           Control.Lens.Operators
import           Paths_lamdu_ide (getDataFileName)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

accessDataFile :: FilePath -> (FilePath -> IO a) -> FilePath -> IO a
accessDataFile startDir accessor fileName =
    (accessor =<< getDataFileName fileName)
    `E.catch` \(E.SomeException _) ->
    accessor $ startDir </> fileName
