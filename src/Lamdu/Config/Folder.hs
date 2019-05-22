{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Lamdu.Config.Folder
    ( Selection(..), _Selection
    , getSelections
    , selectionToPath
    , HasConfigFolder(..)
    , Language, Theme
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Text as Text
import qualified Lamdu.Paths as Paths
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

-- Tags for "tag" in "Selection folder"
data Language
data Theme

newtype Selection folder = Selection Text
    deriving stock (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON)
Lens.makePrisms ''Selection

class HasConfigFolder a where
    type Folder a
    configFolderName :: proxy a -> FilePath

configFolder :: HasConfigFolder a => proxy a -> IO FilePath
configFolder p =
    Paths.getDataFileName "config.json"
    <&> FilePath.takeDirectory <&> (</> configFolderName p)

selectionToPath ::
    HasConfigFolder a => Proxy a -> Selection (Folder a) -> IO FilePath
selectionToPath p (Selection selection) =
    configFolder p <&> (</> Text.unpack selection ++ ".json")

listPaths :: FilePath -> IO [FilePath]
listPaths dir = Directory.getDirectoryContents dir <&> map (dir </>)

getFiles :: HasConfigFolder a => proxy a -> IO [FilePath]
getFiles p =
    configFolder p
    >>= listPaths
    <&> filter ((== ".json") . FilePath.takeExtension)
    <&> filter ((/= ".mixin") . FilePath.takeExtension . FilePath.dropExtension)

pathToSelection :: FilePath -> Selection a
pathToSelection = Selection . Text.pack . FilePath.takeFileName . FilePath.dropExtension

getSelections :: HasConfigFolder a => proxy a -> IO [Selection (Folder a)]
getSelections p = getFiles p <&> map pathToSelection
