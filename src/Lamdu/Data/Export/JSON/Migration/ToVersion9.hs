{-# LANGUAGE TypeFamilies #-}

module Lamdu.Data.Export.JSON.Migration.ToVersion9 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import qualified Data.Map as Map
import           Data.String
import           Data.Text (unpack)
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

collectNominals :: _ -> Either Text (Map _ (Map _ _))
collectNominals obj =
    case obj ^. Lens.at "nom" of
    Just (Aeson.String nomId) ->
        case obj ^. Lens.at "typeParams" of
        Nothing -> nomId ~~> mempty & Right
        Just typeParams ->
            case Aeson.fromJSON typeParams of
            Aeson.Error str -> Left (fromString str)
            Aeson.Success nomParams -> nomId ~~> nomParams & Right
    Just _ -> Left "Malformed 'nom' id"
    Nothing -> Right mempty

migrateNomParams ::
    Map _ (Map _ _) -> Map _ _ ->
    Aeson.Value -> Either Text Aeson.Value
migrateNomParams nomsMap nomParams (Aeson.Object obj) =
    obj ^@.. Lens.ifolded
    & (traverse . _1) migrateNomParam
    >>= (traverse . _2) (migrateVal nomsMap)
    <&> Aeson.object
    where
        migrateNomParam t =
            nomParams ^. Lens.at t & maybe (Left err) Right
            where
                err = "unexpected nom param " <> fromString (show t <> "\n" <> show nomParams)
migrateNomParams _ _ _ = Left "malformed nomParams"

migrateRowVars :: Maybe Aeson.Value -> Aeson.Value -> Either Text Aeson.Value
migrateRowVars mConstraints (Aeson.Array arr) =
    arr ^.. traverse
    & traverse f
    >>= addConstraints
    <&> Aeson.object
    where
        f (Aeson.String x) = Right (fromString (unpack x), Aeson.Array mempty)
        f _ = Left "malformed row var"
        addConstraints x =
            case mConstraints of
            Just (Aeson.Object c) -> c ^@.. Lens.ifolded <> x & Right
            Just{} -> Left "malformed row constraints"
            Nothing -> Right x
migrateRowVars _ _ = Left "malformed rowVars"

migrateVal ::
    Map _ (Map _ _) ->
    Aeson.Value -> Either Text Aeson.Value
migrateVal nomsMap (Aeson.Object obj) =
    case (obj ^. Lens.at "nomId", obj ^. Lens.at "nomParams") of
    (Just (Aeson.String nomId), Just instArgs) ->
        case nomsMap ^. Lens.at nomId of
        Just nomParams ->
            migrateNomParams nomsMap nomParams instArgs
            <&>
            \r ->
            obj
            & Lens.at "nomParams" .~ Nothing
            & Lens.at "nomTypeArgs" ?~ r
        Nothing -> Left ("unexpected nom: " <> nomId)
    _ ->
        case obj ^. Lens.at "schemeBinders" of
        Just (Aeson.Object binders) ->
            Lens._Just
            (migrateRowVars (binders ^? Lens.ix "constraints" . _Object . Lens.ix "rowVars"))
            (binders ^. Lens.at "rowVars")
            <&>
            \rowVars ->
            obj
            & Lens.at "schemeBinders" .~ Nothing
            & Lens.at "typeVars" .~ binders ^. Lens.at "typeVars"
            & Lens.at "rowVars" .~ rowVars
        _ -> pure obj
        >>= traverse (migrateVal nomsMap)
    <&> Aeson.Object
migrateVal nomsMap (Aeson.Array vals) =
    traverse (migrateVal nomsMap) vals <&> Aeson.Array
migrateVal _ x = pure x

typeParamsToTypeVars ::
    (Lens.IxValue a ~ Aeson.Value, IsString (Lens.Index a), Lens.At a) => a -> a
typeParamsToTypeVars x =
    case x ^. Lens.at "typeParams" of
    Just typeParams ->
        x
        & Lens.at "typeParams" .~ Nothing
        & Lens.at "typeVars" ?~ Aeson.toJSON (typeParams ^.. _Object . traverse)
    Nothing -> x

fixFrozenDeps ::
    (Lens.IxValue a ~ Aeson.Value, IsString (Lens.Index a), Lens.At a) => a -> a
fixFrozenDeps =
    Lens.ix "frozenDeps" . _Object . Lens.ix "nominals" . _Object . traverse . _Object
    %~ typeParamsToTypeVars

migrateEntity :: Map _ (Map _ _) -> _ -> Either Text _
migrateEntity nomsMap obj =
    case obj ^. Lens.at "nom" of
    Just{} -> typeParamsToTypeVars obj
    _ -> fixFrozenDeps obj & Lens.ix "repl" . _Object %~ fixFrozenDeps
    & traverse (migrateVal nomsMap)

extraNoms :: Map _ (Map _ _)
extraNoms =
    [ -- Mut (ST)
        ("42493a53540000000000000000000000", [thread, value])
    , -- STArray
        ("42493a73744172726179000000000000", [thread, value])
    , -- STRef
        ("42493a73745265660000000000000000", [thread, value])
    , -- Array
        ("42493a61727261790000000000000000", [value])
    , -- IO
        ("42493a494f0000000000000000000000", [eff])
    ]
    <&> _2 %~ Map.fromList . map (join (,))
    & Map.fromList
    where
        value = "42493a76616c00000000000000000000"
        thread = "73000000000000000000000000000000"
        eff = "65666600000000000000000000000000"

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate =
    migrateToVer 9 $
    \vals ->
    do
        nomsMap <-
            vals ^.. traverse . _Object
            & traverse collectNominals
            <&> (^. traverse)
            -- Add Mut, Array, etc which are opaque (undeclared) nominals with parameters
            <&> (extraNoms <>)
        (traverse . _Object) (migrateEntity nomsMap) vals
