module Lamdu.Data.Export.JSON.Migration.ToVersion8 (migrate) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import           Data.HashMap.Strict (HashMap)
import qualified Data.UUID.Utils as UUIDUtils
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateTerm :: Aeson.Value -> Either Text Aeson.Value
migrateTerm (Aeson.Object x) =
    case (x ^. Lens.at "fromNomId", x ^. Lens.at "fromNomVal", x ^. Lens.at "id" <&> Aeson.fromJSON) of
    (Just nomId, Just nomVal, Just (Aeson.Success i)) ->
        (_Object . traverse) migrateTerm nomVal
        <&>
        \fixedVal ->
        mempty
        & Lens.at "id" ?~ Aeson.toJSON i
        & Lens.at "applyArg" ?~ fixedVal
        & Lens.at "applyFunc" ?~
            ( mempty
            & Lens.at "id" ?~ Aeson.toJSON (UUIDUtils.augment "to-version-8" i)
            & Lens.at "fromNomId" ?~ nomId
            & Aeson.Object
            )
    (Nothing, Nothing, _) -> traverse migrateTerm x
    _ -> Left "Malformed from-nom term"
    <&> Aeson.Object
migrateTerm x = Right x

migrateObj :: HashMap Text Aeson.Value -> Either Text (HashMap Text Aeson.Value)
migrateObj x =
    x
    & Lens.ix "val" migrateTerm
    >>= (Lens.ix "repl" . _Object . Lens.ix "val") migrateTerm

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 8 ((traverse . _Object) migrateObj)
