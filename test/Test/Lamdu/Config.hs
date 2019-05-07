{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Test.Lamdu.Config (loadConfigObject) where

import qualified Control.Monad.Trans.FastWriter as Writer
import           Data.Aeson (FromJSON)
import qualified Data.Aeson.Config as AesonConfig
import           Data.Proxy (Proxy(..))
import           Lamdu.Config.Folder (HasConfigFolder(..))
import qualified Lamdu.Paths as Paths
import           System.FilePath ((</>), takeDirectory)

import           Test.Lamdu.Prelude

loadConfigObject ::
    forall a.
    (FromJSON a, HasConfigFolder a) =>
    FilePath -> IO a
loadConfigObject selection =
    Paths.getDataFileName "config.json"
    <&> takeDirectory
    <&> (\x -> x </> configFolder (Proxy @a) </> (selection <> ".json"))
    >>= Writer.evalWriterT . AesonConfig.load
