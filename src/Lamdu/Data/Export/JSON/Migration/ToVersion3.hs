-- | Migration support for JSONs with schemaVersion 2 to 3
-- Migration changes:
-- 1. Change "schemaVersion" to 3
--
-- 2. Replace "OO"    with {"Object": <TAG>}
--        and "Infix" with {"Infix": [<TAG>, <TAG>]}
--    (presentation modes now mention the special tags)

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Lamdu.Data.Export.JSON.Migration.ToVersion3 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.List.Class (sortOn)
import qualified Data.Vector as Vector

import           Lamdu.Prelude

version :: Integer -> Aeson.Value
version x =
    mempty
    & Lens.at "schemaVersion" ?~ Aeson.toJSON x
    & Aeson.Object

type TagId = Text
type TagOrder = Int

migrateEntity :: Map TagId TagOrder -> Aeson.Value -> Either Text Aeson.Value
migrateEntity tagMap (Aeson.Object obj) =
    obj ^. Lens.at "defPresentationMode" >>= mkNewPresMode mTags
    & fromMaybe (Right obj)
    <&> Aeson.Object
    where
        mkNewPresMode Nothing _ =
            obj
            & Lens.at "defPresentationMode" ?~ Aeson.String "Verbose"
            & Right & Just
        mkNewPresMode (Just tags) (Aeson.String s) | s == "OO" =
            obj
            & Lens.at "defPresentationMode" ?~
                (mempty & Lens.at "Object" ?~ Aeson.String (head tags) & Aeson.Object)
            & Right & Just
        mkNewPresMode (Just tags) (Aeson.String s) | s == "Infix" =
            obj
            & Lens.at "defPresentationMode" ?~
                (mempty & Lens.at "Infix" ?~ (take 2 tags <&> Aeson.String & Vector.fromList & Aeson.Array) & Aeson.Object)
            & Right & Just
        mkNewPresMode _ _ = Nothing
        mTags = mRecordType <&> HashMap.keys <&> sortOn tagOrder
        tagOrder t = tagMap ^. Lens.at t
        mRecordType =
            obj ^. Lens.at "typ"
            >>= mObject
            >>= (^. Lens.at "schemeType")
            >>= mObject
            >>= (^. Lens.at "funcParam")
            >>= mObject
            >>= (^. Lens.at "record")
            >>= mObject
        -- TODO: something like this must exist
        mObject (Aeson.Object x) = Just x
        mObject _ = Nothing
migrateEntity _ _ = Left "Expecting object"

collectTags :: Aeson.Value -> Either Text (Map TagId TagOrder)
collectTags (Aeson.Object obj) =
    case obj ^. Lens.at "tag" of
    Just (Aeson.String tagId) ->
        case obj ^. Lens.at "tagOrder" of
        Nothing -> Left "Malformed 'tag' node"
        Just (Aeson.Number tagOrder) -> mempty & Lens.at tagId ?~ round tagOrder & Right
        Just _ -> Left "Malformed 'tagOrder'"
    Just _ -> Left "Malformed 'tag' id"
    Nothing -> Right mempty
collectTags _ = Right mempty

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate (Aeson.Array vals)
    | Vector.head vals == version 2 =
        do
            tagMap <- traverse collectTags vals <&> (^. traverse)
            traverse (migrateEntity tagMap) (Vector.tail vals)
        <&> (,) (version 3)
        <&> (Lens._Cons #)
        <&> Aeson.Array
    | otherwise =
        Left "Migrating from incorrent version"
migrate _ = Left "top-level should be array"
