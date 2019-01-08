module Lamdu.Data.Export.JSON.Migration.ToVersion5 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Lamdu.Data.Export.JSON.Migration.Common (version)

import           Lamdu.Prelude

replaceKey :: Lens.At t => Lens.Index t -> Lens.Index t -> t -> t
replaceKey pre post obj =
    case obj ^. Lens.at pre of
    Nothing -> obj
    Just v ->
        obj
        & Lens.at pre .~ Nothing
        & Lens.at post ?~ v

migrateVal :: Aeson.Value -> Aeson.Value
migrateVal (Aeson.Object obj) =
    obj <&> migrateVal
    & replaceKey "sum" "variant"
    & replaceKey "sumTypeVars" "variantTypeVars"
    & Aeson.Object
migrateVal (Aeson.Array vals) = vals <&> migrateVal & Aeson.Array
migrateVal (Aeson.String x) = Aeson.String x
migrateVal (Aeson.Number x) = Aeson.Number x
migrateVal (Aeson.Bool x) = Aeson.Bool x
migrateVal Aeson.Null = Aeson.Null

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate (Aeson.Array vals) =
    case vals ^? Lens._Cons of
    Just (ver, rest)
        | ver == version 4 ->
            Lens._Cons # (version 5, rest <&> migrateVal) & Aeson.Array & Right
        | otherwise -> Left "Migration from unexpected version"
    _ -> Left "Array of at least 1 element required"
migrate _ = Left "top-level should be array"
