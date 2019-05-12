{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lamdu.Config.Folder
    ( Selection(..), _Selection
    , getFiles, getNames
    , HasConfigFolder(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson (FromJSON(..), ToJSON(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import qualified Lamdu.Paths as Paths
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

newtype Selection a = Selection Text
    deriving stock (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON)
Lens.makePrisms ''Selection

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
            <&> filter ((/= ".mixin") . FilePath.takeExtension . FilePath.dropExtension)
            <&> map (dir </>)

getNames :: forall a. HasConfigFolder a => IO [Selection a]
getNames = getFiles (Proxy @a) <&> map (Selection . Text.pack . FilePath.takeFileName . FilePath.dropExtension)
