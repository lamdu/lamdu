-- | Migration support for JSONs with schemaVersion 2 to 3
-- Migration changes:
-- 1. Change "schemaVersion" to 3
--
-- 2. Replace "OO"    with {"Object": <TAG>}
--        and "Infix" with {"Infix": [<TAG>, <TAG>]}
--    (presentation modes now mention the special tags)

module Lamdu.Data.Export.JSON.Migration.ToVersion3 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.List.Class (sortOn)
import qualified Data.Vector as Vector
import qualified Lamdu.Data.Export.JSON.Migration.Common as Migration

import           Lamdu.Prelude

migrateEntity :: Migration.TagMap -> Aeson.Value -> Either Text Aeson.Value
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
                Aeson.Object ("Object" ~~> Aeson.String (head tags))
            & Right & Just
        mkNewPresMode (Just tags) (Aeson.String s) | s == "Infix" =
            obj
            & Lens.at "defPresentationMode" ?~
                Aeson.Object ("Infix" ~~> (take 2 tags <&> Aeson.String & Vector.fromList & Aeson.Array))
            & Right & Just
        mkNewPresMode _ _ = Nothing
        mTags = mRecordType <&> (^.. Lens.ifolded . Lens.asIndex) <&> sortOn tagOrder
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

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate =
    Migration.migrateToVer 3 $
    \vals ->
    do
        tagMap <- traverse Migration.collectTags vals <&> (^. traverse)
        traverse (migrateEntity tagMap) vals
