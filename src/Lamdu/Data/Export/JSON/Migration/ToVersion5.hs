module Lamdu.Data.Export.JSON.Migration.ToVersion5 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

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
migrateVal x = x

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 5 (pure . fmap migrateVal)
