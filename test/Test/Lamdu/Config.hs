{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Test.Lamdu.Config (loadConfigObject) where

import qualified Control.Monad.Trans.FastWriter as Writer
import           Data.Aeson (FromJSON)
import qualified Data.Aeson.Config as AesonConfig
import           Lamdu.Config.Folder (HasConfigFolder(..), Folder)
import qualified Lamdu.Config.Folder as Folder

import           Test.Lamdu.Prelude

loadConfigObject ::
    (FromJSON a, HasConfigFolder a) =>
    Proxy a -> Folder.Selection (Folder a) -> IO a
loadConfigObject p selection =
    Folder.selectionToPath p selection >>= Writer.evalWriterT . AesonConfig.load
