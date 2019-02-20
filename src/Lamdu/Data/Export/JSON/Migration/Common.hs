module Lamdu.Data.Export.JSON.Migration.Common
    ( version, migrateToVer
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson

import           Lamdu.Prelude

version :: Integer -> Aeson.Value
version x =
    mempty
    & Lens.at "schemaVersion" ?~ Aeson.toJSON x
    & Aeson.Object

migrateToVer ::
    Integer ->
    (Aeson.Array -> Either Text Aeson.Array) ->
    Aeson.Value -> Either Text Aeson.Value
migrateToVer num migrate (Aeson.Array vals) =
    case vals ^? Lens._Cons of
    Just (ver, rest)
        | ver == version (num - 1) ->
            migrate rest
            <&> (,) (version num)
            <&> (Lens._Cons #)
            <&> Aeson.Array
        | otherwise -> Left "Migration from unexpected version"
    _ -> Left "Array of at least 1 element required"
migrateToVer _ _ _ = Left "top-level should be array"
