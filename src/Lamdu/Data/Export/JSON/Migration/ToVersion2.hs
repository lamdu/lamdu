-- | Migration support for JSONs with schemaVersion 1 to 2
-- Migration changes:
-- 1. Change "schemaVersion" to 2
--
-- 2. Replace ["Infix",?] with "Infix"
--    (precedence is now decided by the operator char instead)

module Lamdu.Data.Export.JSON.Migration.ToVersion2 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Foldable (asum)
import qualified Data.Vector as Vector
import           Lamdu.Data.Export.JSON.Migration.Common (version)

import           Lamdu.Prelude

migrateEntity ::
    Aeson.Value -> Either Text Aeson.Value
migrateEntity (Aeson.Object obj) =
    asum
    [ obj ^. Lens.at "defPresentationMode" >>=
        \case
        Aeson.Array vals ->
            case Vector.toList vals of
            [Aeson.String "Infix", Aeson.Number _prec] ->
                obj
                & Lens.at "defPresentationMode" ?~ "Infix"
                & Right
            _ -> Left "malformed presenetation mode"
            & Just
        _ -> Nothing
    ]
    & fromMaybe (Right obj)
    <&> Aeson.Object
migrateEntity _ = Left "Expecting object"

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate (Aeson.Array vals)
    | Vector.head vals == version 1 =
        Vector.tail vals
        & traverse migrateEntity
        <&> (,) (version 2)
        <&> (Lens._Cons #)
        <&> Aeson.Array
    | otherwise =
        Left "Migrating from incorrent version"
migrate _ = Left "top-level should be array"
