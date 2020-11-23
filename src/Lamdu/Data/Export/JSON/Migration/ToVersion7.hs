module Lamdu.Data.Export.JSON.Migration.ToVersion7 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object, _String)
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateVal :: Aeson.Value -> Either Text Aeson.Value
migrateVal val =
    case val ^? _Object . Lens.ix "nomType" . _String of
    Nothing -> Right val
    Just x
        | x == "OpaqueNominal" ->
            val & _Object %~
            (Lens.at "nomType" .~ Nothing) . (Lens.at "typeParams" .~ Nothing)
            & Right
        | otherwise -> Left ("Unexpected nomType: " <> x)

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 7 (traverse migrateVal)
