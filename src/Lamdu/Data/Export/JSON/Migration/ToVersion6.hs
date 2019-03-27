module Lamdu.Data.Export.JSON.Migration.ToVersion6 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.List (sortOn)
import           Data.Vector.Lens (vector)
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateVal :: Aeson.Value -> Either Text Aeson.Value
migrateVal (Aeson.Object obj) =
    case (obj ^. Lens.at "recordTypeVars", obj ^. Lens.at "variantTypeVars") of
    (Nothing, Nothing) -> pure obj
    (Just x, Nothing) -> rowVars x
    (Nothing, Just x) -> rowVars x
    (Just (Aeson.Array rs), Just (Aeson.Array vs)) ->
        rs <> vs & Lens.from vector %~ sortOn show & Aeson.Array & rowVars
    (Just (Aeson.Object rs), Just (Aeson.Object vs)) ->
        rs <> vs & Aeson.Object & rowVars
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
