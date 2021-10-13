{-# LANGUAGE TypeFamilies #-}

module Lamdu.Data.Export.JSON.Migration.ToVersion14 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import qualified Data.UUID.Utils as UUIDUtils
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateTerm :: Aeson.Value -> Either Text Aeson.Value
migrateTerm (Aeson.Object x) =
    case (x ^. Lens.at "id" <&> Aeson.fromJSON, p "injectTag" "injectVal", p "getFieldName" "getFieldRec") of
    (Just (Aeson.Success i), Just f, Nothing) -> r i "inject" f
    (Just (Aeson.Success i), Nothing, Just f) -> r i "getField" f
    (_, Nothing, Nothing) -> traverse migrateTerm x
    _ -> Left "Malformed from-nom term"
    <&> Aeson.Object
    where
        p k0 k1 = (,) <$> x ^. Lens.at k0 <*> x ^. Lens.at k1
        r i k (tag, val) =
            migrateTerm val <&>
            \fixedVal ->
            "id" ~~> Aeson.toJSON i
            <> "applyArg" ~~> fixedVal
            <> "applyFunc" ~~>
                Aeson.Object
                ( "id" ~~> Aeson.toJSON (UUIDUtils.augment "to-version-14" i)
                <> k ~~> tag
                )
migrateTerm x = Right x

migrateObj :: Aeson.Object -> Either Text Aeson.Object
migrateObj x =
    x
    & Lens.ix "val" migrateTerm
    >>= (Lens.ix "repl" . _Object . Lens.ix "val") migrateTerm

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 14 ((traverse . _Object) migrateObj)
