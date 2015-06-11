module Lamdu.DataFile
    ( accessDataFile
    ) where

import qualified Control.Exception as E
import           Paths_lamdu_ide (getDataFileName)
import           System.FilePath ((</>))

accessDataFile :: FilePath -> (FilePath -> IO a) -> FilePath -> IO a
accessDataFile startDir accessor fileName =
    (accessor =<< getDataFileName fileName)
    `E.catch` \(E.SomeException _) ->
    accessor $ startDir </> fileName
