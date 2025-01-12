module Lamdu.Data.Export.JSON.Migration.ToVersion13 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Instances ()
import           Data.Aeson.Lens (_Array, _Object, _String)
import           Data.Binary.Extended (encodeS)
import           Data.UUID (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import qualified Data.Vector as Vector
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

type LamId = Text
type TagId = Text
type LamParamsMap = Map LamId [TagId]

collectLamParams :: Aeson.Value -> Either Text LamParamsMap
collectLamParams (Aeson.Object obj) =
    case (obj ^? Lens.ix "lamId", obj ^? Lens.ix "lamFieldParams" . _Array) of
    (Just (Aeson.String lamId), Just raw) ->
        case traverse (^? _String) raw of
        Nothing -> Left "Malformed 'lamFieldParams'"
        Just params -> mempty & Lens.at lamId ?~ params ^.. Lens.folded & Right
    _ -> Right mempty
collectLamParams _ = Right mempty

encodeParamList :: UUID -> Maybe [TagId] -> Aeson.Object
encodeParamList _ Nothing = mempty
encodeParamList baseId (Just params) =
    "record" ~~>
    (Aeson.Array . Vector.fromList)
    ((zip [0 :: Int ..] params <&> uncurry mkField) <> [rowTail])
    where
        rowTail =
            "rowId" ~~> Aeson.toJSON (UUIDUtils.augment "tail" baseId)
            & Aeson.Object
        mkField i tagId =
            mempty
            & addId "id"
            & addId "rowId"
            & Lens.at "rowTag" ?~ Aeson.String tagId
            & Aeson.Object
            where
                addId :: _ -> Aeson.Object -> _
                addId t x = x & Lens.at t ?~ Aeson.toJSON (UUIDUtils.augment (encodeS (i, t)) baseId)

migrateExpr :: LamParamsMap -> _ -> Either Text _
migrateExpr lamsMap obj =
    (traverse . _Object) (migrateExpr lamsMap) obj <&>
    case (obj ^? Lens.ix "lamVar", obj ^? Lens.ix "id" . _String, obj ^. Lens.at "id" <&> Aeson.fromJSON) of
    (Just{}, Just lamId, Just (Aeson.Success lamExprId))->
        Lens.at "lamParamType" ?~
        ( encodeParamList typId (lamsMap ^. Lens.at lamId)
            & Lens.at "id" ?~ Aeson.toJSON typId
            & Aeson.Object
        )
        where
            typId = UUIDUtils.augment "to-version-13" lamExprId
    _ -> id

migrateRowFields :: Aeson.Object -> Either Text [Aeson.Value]
migrateRowFields obj =
    traverse (uncurry mkField) (obj ^@.. Lens.ifolded)
    where
        mkField key (Aeson.Object val) =
            val
            & Lens.at "rowTag" ?~ Aeson.toJSON key
            & Aeson.Object & Right
        mkField _ _ = Left "Malformed row item"

migrateRow :: Aeson.Value -> Either Text Aeson.Value
migrateRow (Aeson.Object obj) =
    migrateRowFields obj <&> Aeson.Array . Vector.fromList
migrateRow (Aeson.Array v) =
    case v ^.. traverse of
    [Aeson.Object obj, Aeson.String rest] ->
        migrateRowFields obj
        <&> (<> ["rowVar" ~~> Aeson.String rest & Aeson.Object])
        <&> Aeson.Array . Vector.fromList
    _ -> Left "Malformed row"
migrateRow _ = Left "Malformed row"

migrateType :: _ -> Either Text _
migrateType obj =
    (traverse . _Object) migrateType obj
    >>= (traverse . _Array . traverse . _Object) migrateType
    >>= Lens.ix "variant" migrateRow
    >>= Lens.ix "record" migrateRow

migrateScheme :: Aeson.Value -> Either Text Aeson.Value
migrateScheme (Aeson.Object obj) = obj & (Lens.ix "schemeType" . _Object) migrateType <&> Aeson.Object
migrateScheme _ = Left "Malformed scheme"

migrateFrozenDeps :: Aeson.Value -> Either Text _
migrateFrozenDeps obj =
    obj
    & (Lens.ix "defTypes" . _Object . traverse) migrateScheme
    >>= (Lens.ix "nominals" . _Object . traverse . _Object . Lens.ix "nomType") migrateScheme

migrateEntity :: LamParamsMap -> Aeson.Value -> Either Text [Aeson.Value]
migrateEntity lamsMap (Aeson.Object obj)
    | Lens.has (Lens.ix "lamId") obj =
        Right
        [ obj
            & Lens.at "lamFieldParams" .~ Nothing
            & Lens.at "lamId" .~ Nothing
            & Aeson.Object
        ]
    | otherwise =
        obj
        & (Lens.ix "val" . _Object) (migrateExpr lamsMap)
        >>= (Lens.ix "frozenDeps") migrateFrozenDeps
        >>= (Lens.ix "repl" . _Object . Lens.ix "val" . _Object) (migrateExpr lamsMap)
        >>= (Lens.ix "repl" . _Object . Lens.ix "frozenDeps") migrateFrozenDeps
        >>= Lens.ix "typ" migrateScheme
        >>= Lens.ix "nomType" migrateScheme
        <&> (:[]) . Aeson.Object
migrateEntity _ x = Right [x]

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate =
    migrateToVer 13 $
    \xs ->
    do
        lamsMap <- traverse collectLamParams xs <&> (^. traverse)
        traverse (migrateEntity lamsMap) xs <&> Vector.fromList . concat
