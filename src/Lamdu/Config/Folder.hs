module Lamdu.Config.Folder
    ( Selection, getFiles, getNames
    , HasConfigFolder(..)
    ) where

import qualified Data.Text as Text
import qualified Lamdu.Paths as Paths
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

type Selection = Text

class HasConfigFolder a where
    configFolder :: proxy a -> FilePath

getFiles :: HasConfigFolder a => proxy a -> IO [FilePath]
getFiles p =
    do
        dir <-
            Paths.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> configFolder p)
        Directory.getDirectoryContents dir
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (dir </>)

getNames :: HasConfigFolder a => proxy a -> IO [Selection]
getNames p = getFiles p <&> map (Text.pack . FilePath.takeFileName . FilePath.dropExtension)
