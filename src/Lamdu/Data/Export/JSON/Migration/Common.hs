module Lamdu.Data.Export.JSON.Migration.Common
    ( collectTags, TagId, TagOrder, TagMap
    , version, migrateToVer
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import           Lamdu.Prelude

type TagId = Text
type TagOrder = Int

type TagMap = Map TagId TagOrder

collectTags :: Aeson.Value -> Either Text TagMap
collectTags (Aeson.Object obj) =
    case obj ^. Lens.at "tagOrder" of
    Just (Aeson.Number tagOrder) ->
        case obj ^. Lens.at "tag" of
        Nothing -> Left "Malformed 'tag' node"
        Just (Aeson.String tagId) -> tagId ~~> round tagOrder & Right
        Just _ -> Left "Malformed 'tagOrder'"
    Just x -> Left $ "Malformed 'tag' id: " <> Text.pack (show x)
    Nothing -> Right mempty
collectTags _ = Right mempty

version :: Integer -> Aeson.Value
version x = "schemaVersion" ~~> Aeson.toJSON x & Aeson.Object

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
