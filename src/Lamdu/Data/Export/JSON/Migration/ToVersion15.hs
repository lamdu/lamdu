{-# LANGUAGE TypeFamilies #-}

module Lamdu.Data.Export.JSON.Migration.ToVersion15 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object, _Array, _String, key, members, values)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

extend :: Text -> Text
extend =
    Text.take uuidHexLen . (<> Text.replicate uuidHexLen "0")
    where
        uuidHexLen = 32

rekey :: Aeson.Value -> Aeson.Value
rekey x =
    x ^@.. members
    <&> _1 %~ extend
    & Aeson.object

-- When the scheme belongs to a nominal decl, each of the extended
-- nominal type/row params needs the same extension when used inside
-- the scheme
migrateScheme :: Set Text -> Aeson.Value -> Aeson.Value
migrateScheme needExtension =
    key "schemeType" %~ eachObj
    where
        shouldExtend x = needExtension ^. Lens.contains x
        extendVar =
            _String . Lens.filtered shouldExtend %~ extend
        eachObj x =
            x
            & key "typeVar" %~ extendVar
            & key "rowVar" %~ extendVar
            & key "nomTypeArgs" %~ rekey
            & key "nomRowArgs" %~ rekey
            & members %~ eachObj
            & values %~ eachObj

jsonStrings :: Lens.Traversal' Aeson.Value Text
jsonStrings = _Array . traverse . _String

migrateNomDecl :: Aeson.Object -> Aeson.Object
migrateNomDecl x =
    x
    & Lens.ix "typeVars" . jsonStrings %~ extend
    & Lens.ix "rowVars" . jsonStrings %~ extend
    & Lens.ix "nomType" %~ migrateScheme nomVars
    where
        nomVars =
            x ^.. (Lens.ix "typeVars" <> Lens.ix "rowVars") .
            jsonStrings & Set.fromList

migrateFrozen :: Aeson.Value -> Aeson.Value
migrateFrozen x =
    x
    & key "nominals" . members . _Object %~ migrateNomDecl
    & key "defTypes" . members %~ migrateScheme mempty

migrateObj :: Aeson.Object -> Aeson.Object
migrateObj x
    | Lens.has (Lens.ix "nom") x = migrateNomDecl x
    | otherwise =
        x
        & Lens.ix "typ" %~ migrateScheme mempty
        & Lens.ix "frozenDeps" %~ migrateFrozen
        & Lens.ix "repl" . key "frozenDeps" %~ migrateFrozen

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate =
    migrateToVer 15 (pure . (traverse . _Object %~ migrateObj))
