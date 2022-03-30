module Lamdu.Data.Export.JSON.Migration.ToVersion12 (migrate) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import           Data.Aeson.Lens (key, members, _Array)
import           Data.List (sortOn)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Lamdu.Data.Export.JSON.Migration.Common as Migration

import           Lamdu.Prelude

operatorPresentationMode :: Aeson.Array -> Aeson.Value
operatorPresentationMode tags = "Operator" ~~> Aeson.Array tags & Aeson.Object

migrateObject ::
    Migration.TagMap -> Aeson.Object -> Aeson.Value -> Either Text Aeson.Array
migrateObject tagMap defObj (Aeson.String objTag) =
    case defObj ^? recordType of
    Just recordObj ->
        recordObj ^@.. members
        <&> Aeson.Key.toText . fst
        & filter (/= objTag)
        & sortOn tagOrder
        & objTags
    Nothing ->
        "def with 'Object' presentation mode does not have json key typ.schemeType.funcParam.record" <> Text.pack (show defObj)
        & Left
    where
        objTags [] = Left "Object presentation mode without any record fields"
        objTags (secondTag:_) =
            [objTag, secondTag] <&> Aeson.String & Vector.fromList & Right
        recordType =
            Lens.ix "typ" . key "schemeType" . key "funcParam" . key "record"
        tagOrder :: Migration.TagId -> Maybe Migration.TagOrder
        tagOrder tag = tagMap ^. Lens.at tag

migrateObject _ _ x =
    "Unexpected 'Object' presentation mode: " <> Text.pack (show x) & Left

migratePresentationMode ::
    Migration.TagMap -> Aeson.Value -> Either Text Aeson.Value
migratePresentationMode tagMap (Aeson.Object obj) =
    case obj ^. Lens.at "defPresentationMode" of
    Nothing -> Right obj
    Just (Aeson.String "Verbose") -> Right obj
    Just (Aeson.Object pModeObj)
        | length pModeObj == 1 ->
            (pModeObj ^? Lens.ix "Infix" . _Array <&> Right)
            <|> (pModeObj ^. Lens.at "Object" <&> migrateObject tagMap obj)
            & fromMaybe (err pModeObj)
            <&> operatorPresentationMode
            <&> intoObj
    Just x -> err x
    <&> Aeson.Object
    where
        intoObj presMode = obj & Lens.at "defPresentationMode" ?~ presMode
        err x = "Unexpected presentation mode: " <> Text.pack (show x) & Left
migratePresentationMode _ x = Right x

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate =
    Migration.migrateToVer 12 $
    \vals ->
    do
        tagMap <- traverse Migration.collectTags vals <&> (^. traverse)
        traverse (migratePresentationMode tagMap) vals
