module Lamdu.Data.Export.JSON.Migration.ToVersion10 (migrate) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

isOperator :: Text -> Bool
isOperator = Lens.allOf Lens.each (`elem` Chars.operator)

migrateVal :: Aeson.Value -> Either Text Aeson.Value
migrateVal (Aeson.Object val) =
    case val ^. Lens.at "name" of
    Nothing -> Right val
    Just (Aeson.String x) ->
        val
        & Lens.at "name" .~ Nothing
        & addName
        & Right
        where
            addName
                | isOperator x = Lens.at "op" ?~ Aeson.String x
                | otherwise =
                    Lens.at "names" ?~
                    Aeson.Object ("english" ~~> Aeson.String x)
    Just x -> Left ("Unexpected name: " <> Text.pack (show x))
    <&> Aeson.Object
migrateVal x = Right x

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 10 (traverse migrateVal)
