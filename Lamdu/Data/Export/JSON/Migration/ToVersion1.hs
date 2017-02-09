-- | Migration support for JSONs with version 0 (no "schemaVersion") to version 1
-- Migration changes:
-- 1. Add {"schemaVersion":1} as head of list
--
-- 2. Move "frozenDefTypes" into "frozenDeps"."defTypes"
--
-- 3. Copy all nominal types into "frozenDeps"."nominals" inside all
-- definitions that use them

{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Data.Export.JSON.Migration.ToVersion1 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Lamdu.Prelude

version1 :: Aeson.Value
version1 =
    mempty
    & Lens.at "schemaVersion" ?~ Aeson.Number 1
    & Aeson.Object

type NominalId = Text
type FrozenNominal = Aeson.Value

children :: Aeson.Value -> [Aeson.Value]
children (Aeson.Object x) = x ^.. Lens.folded
children (Aeson.Array  x) = x ^.. Lens.folded
children Aeson.Null {} = []
children Aeson.Bool {} = []
children Aeson.Number {} = []
children Aeson.String {} = []

scanNomIds :: Aeson.Value -> Either Text (Set NominalId)
scanNomIds val =
    (<>)
    <$> recurse
    <*> case val of
        Aeson.Object obj ->
            (<>)
            <$> asSet (obj ^. Lens.at "fromNomId")
            <*> asSet (obj ^. Lens.at "toNomId")
        _ -> pure mempty
    where
        recurse :: Either Text (Set NominalId)
        recurse = children val & traverse scanNomIds <&> mconcat
        asSet Nothing = Right mempty
        asSet (Just (Aeson.String nomId)) = Set.singleton nomId & Right
        asSet (Just _) = Left "fromNomId/toNomId must be a string"

setMapIntersection :: Ord k => Set k -> Map k a -> Map k a
setMapIntersection s m = m `Map.intersection` Map.fromSet (const ()) s

addFrozenDeps ::
    Map NominalId FrozenNominal -> Aeson.Value -> Aeson.Object ->
    Either Text Aeson.Object
addFrozenDeps nominalMap frozenDefTypes defObj =
    do
        usedNoms <-
            case defObj ^. Lens.at "val" of
            Nothing -> Left "definition with no val"
            Just val -> scanNomIds val
        unless (Set.null (usedNoms `Set.difference` Map.keysSet nominalMap))
            (Left "undefined noms used")
        let frozenNominals =
                setMapIntersection usedNoms nominalMap
                & Aeson.toJSON
        let frozenDeps =
                mempty
                & Lens.at "defTypes" ?~ frozenDefTypes
                & Lens.at "nominals" ?~ frozenNominals
                & Aeson.Object
        defObj
            & Lens.at "frozenDeps" ?~ frozenDeps
            & pure

convertBuiltin :: Aeson.Value -> Aeson.Object -> Either Text Aeson.Object
convertBuiltin builtin obj =
    do
        bobj <-
            case builtin of
            Aeson.Object x -> return x
            _ -> Left "builtin not an object"
        name <-
            case bobj ^. Lens.at "name" of
            Nothing -> Left "builtin with no name"
            Just x -> return x
        scheme <-
            case bobj ^. Lens.at "scheme" of
            Nothing -> Left "builin with no scheme"
            Just x -> return x
        obj
            & Lens.at "builtin" ?~ name
            & Lens.at "typ" ?~ scheme
            & pure

migrateEntity ::
    Map NominalId FrozenNominal -> Aeson.Value -> Either Text Aeson.Value
migrateEntity nominalMap (Aeson.Object obj) =
    do
        case obj ^. Lens.at "schemaVersion" of
            Nothing -> return ()
            Just _ -> "found unexpected version" & Left & Left
        case obj ^. Lens.at "frozenDefTypes" of
            Nothing -> return ()
            Just frozenDefTypes ->
                obj
                & Lens.at "frozenDefTypes" .~ Nothing
                & addFrozenDeps nominalMap frozenDefTypes
                & Left
        case obj ^. Lens.at "builtin" of
            Nothing -> return ()
            Just builtin -> convertBuiltin builtin obj & Left
        return obj
    & either id return
    <&> Aeson.Object
migrateEntity _ _ = Left "Expecting object"

collectNominals :: Aeson.Value -> Either Text (Map NominalId FrozenNominal)
collectNominals (Aeson.Object obj) =
    case obj ^. Lens.at "nom" of
    Just (Aeson.String nomId) ->
        case obj ^. Lens.at "nomType" of
        Nothing -> Left "Malformed 'nom' node"
        Just nomType ->
            mempty & Lens.at nomId ?~ frozenNom & Right
            where
                frozenNom =
                    mempty
                    & Lens.at "nomType" ?~ nomType
                    & Lens.at "typeParams" .~ (obj ^. Lens.at "typeParams")
                    & Aeson.Object
    Just _ -> Left "Malformed 'nom' id"
    Nothing -> Right mempty
collectNominals _ = Right mempty

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate (Aeson.Array vals) =
    do
        nominalMap <- traverse collectNominals vals <&> (^. traverse)
        newVals <- traverse (migrateEntity nominalMap) vals
        Lens._Cons # (version1, newVals)
            & Aeson.Array & pure
migrate _ = Left "top-level should be array"
