module Lamdu.Data.Export.JSON.Migration.ToVersion9 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.String
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

type NominalId = Text

type NominalParams = Map Text Text

collectNominals :: Aeson.Object -> Either Text (Map NominalId NominalParams)
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

migrateNomParam ::
    NominalParams ->
    Text -> Either Text Text
migrateNomParam nomParams t =
    nomParams ^. Lens.at t
    & maybe (Left ("unexpected nom param " <> t <> fromString ("\n" <> show nomParams))) Right

migrateNomParams ::
    Map NominalId NominalParams -> NominalParams ->
    Aeson.Value -> Either Text Aeson.Value
migrateNomParams nomsMap nomParams (Aeson.Object obj) =
    obj ^@.. Lens.ifolded
    & (traverse . _1) (migrateNomParam nomParams)
    >>= (traverse . _2) (migrateVal nomsMap)
    <&> Aeson.object
migrateNomParams _ _ _ = Left "malformed nomParams"

migrateRowVars :: Maybe Aeson.Value -> Aeson.Value -> Either Text Aeson.Value
migrateRowVars mConstraints (Aeson.Array arr) =
    arr ^.. traverse
    & traverse f
    <&> HashMap.fromList
    >>= addConstraints
    <&> Aeson.Object
    where
        f (Aeson.String x) = Right (x, Aeson.Array mempty)
        f _ = Left "malformed row var"
        addConstraints x =
            case mConstraints of
            Just (Aeson.Object c) -> c <> x & Right
            Just{} -> Left "malformed row constraints"
            Nothing -> Right x
migrateRowVars _ _ = Left "malformed rowVars"

migrateVal ::
    Map NominalId NominalParams ->
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

typeParamsToTypeVars :: Aeson.Object -> Aeson.Object
typeParamsToTypeVars x =
    case x ^. Lens.at "typeParams" of
    Just typeParams ->
        x
        & Lens.at "typeParams" .~ Nothing
        & Lens.at "typeVars" ?~ Aeson.toJSON (typeParams ^.. _Object . traverse)
    Nothing -> x

migrateEntity ::
    Map NominalId NominalParams ->
    Aeson.Object -> Either Text Aeson.Object
migrateEntity nomsMap obj =
    case obj ^. Lens.at "nom" of
    Just{} -> typeParamsToTypeVars obj
    _ ->
        fixFrozenDeps obj
        & Lens.ix "repl" . _Object %~ fixFrozenDeps
    & traverse (migrateVal nomsMap)
    where
        fixFrozenDeps =
            Lens.ix "frozenDeps" . _Object . Lens.ix "nominals" . _Object . traverse . _Object
            %~ typeParamsToTypeVars

extraNoms :: Map NominalId NominalParams
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
