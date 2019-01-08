module Lamdu.Data.Export.JSON.Migration.Common (version) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson

import           Lamdu.Prelude

version :: Integer -> Aeson.Value
version x =
    mempty
    & Lens.at "schemaVersion" ?~ Aeson.toJSON x
    & Aeson.Object
