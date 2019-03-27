module Lamdu.Data.Export.JSON.Migration.ToVersion6 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateVal :: Aeson.Value -> Either Text Aeson.Value
migrateVal (Aeson.Object obj) =
    case (obj ^. Lens.at "recordTypeVars", obj ^. Lens.at "variantTypeVars") of
    (Nothing, Nothing) -> pure obj
    (Just x, Nothing) -> rowVars x
    (Nothing, Just x) -> rowVars x
    (Just (Aeson.Array rs), Just (Aeson.Array vs)) -> rowVars (Aeson.Array (rs <> vs))
    (Just (Aeson.Object rs), Just (Aeson.Object vs)) -> rowVars (Aeson.Object (rs <> vs))
    _ -> Left "Unexpected format of row vars"
    >>= traverse migrateVal <&> Aeson.Object
    where
        rowVars x =
            obj
            & Lens.at "recordTypeVars" .~ Nothing
            & Lens.at "variantTypeVars" .~ Nothing
            & Lens.at "rowVars" ?~ x
            & pure
migrateVal (Aeson.Array vals) = traverse migrateVal vals <&> Aeson.Array
migrateVal x = pure x

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 6 (traverse migrateVal)
