module Lamdu.Data.Export.JSON.Migration.ToVersion11 (migrate) where

import qualified Data.Aeson as Aeson
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrate :: Aeson.Value -> Either Text Aeson.Value
-- Codec version 11 extends version 10.
-- So the "migration" just changes the version number
migrate = migrateToVer 11 Right
