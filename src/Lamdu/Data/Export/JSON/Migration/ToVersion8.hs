module Lamdu.Data.Export.JSON.Migration.ToVersion8 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import qualified Data.UUID.Utils as UUIDUtils
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateTerm :: Aeson.Value -> Either Text Aeson.Value
migrateTerm (Aeson.Object x) =
    case (x ^. Lens.at "fromNomId", x ^. Lens.at "fromNomVal", x ^. Lens.at "id" <&> Aeson.fromJSON) of
    (Just nomId, Just nomVal, Just (Aeson.Success i)) ->
        migrateTerm nomVal <&>
        \fixedVal ->
        "id" ~~> Aeson.toJSON i
        <> "applyArg" ~~> fixedVal
        <> "applyFunc" ~~>
            Aeson.Object
            ( "id" ~~> Aeson.toJSON (UUIDUtils.augment "to-version-8" i)
            <> "fromNomId" ~~> nomId
            )
    (Nothing, Nothing, _) -> traverse migrateTerm x
    _ -> Left "Malformed from-nom term"
    <&> Aeson.Object
migrateTerm x = Right x

migrateObj :: Aeson.Object -> Either Text Aeson.Object
migrateObj x =
    x
    & Lens.ix "val" migrateTerm
    >>= (Lens.ix "repl" . _Object . Lens.ix "val") migrateTerm

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 8 ((traverse . _Object) migrateObj)
