-- | Migrate any older version to current
module Lamdu.Data.Export.JSON.Migration
    ( migrateAsNeeded
    , currentVersion
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Text (unpack)
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion1 as ToVersion1
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion2 as ToVersion2
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion3 as ToVersion3
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion4 as ToVersion4
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion5 as ToVersion5
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion6 as ToVersion6
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion7 as ToVersion7
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion8 as ToVersion8

import           Lamdu.Prelude

getVersion :: Aeson.Value -> Either Text Int
getVersion (Aeson.Array values) =
    case values ^? Lens.ix 0 of
        Just (Aeson.Object obj) ->
            case obj ^. Lens.at "schemaVersion" of
            Nothing -> Right 0
            Just (Aeson.Number ver) -> truncate ver & Right
            Just _ -> Left "schemaVersion must be a number"
        Just _ -> Left "Expecting top-level array items to be objects"
        Nothing -> Left "Empty document"
getVersion _ = Left "Expecting top-level array"

versionMigrations :: [Aeson.Value -> Either Text Aeson.Value]
versionMigrations =
    [ ToVersion1.migrate
    , ToVersion2.migrate
    , ToVersion3.migrate
    , ToVersion4.migrate
    , ToVersion5.migrate
    , ToVersion6.migrate
    , ToVersion7.migrate
    , ToVersion8.migrate
    ]

currentVersion :: Int
currentVersion = length versionMigrations

toIO :: Either Text a -> IO a
toIO = either (fail . unpack) pure

applyMigrations :: Aeson.Value -> Int -> IO Aeson.Value
applyMigrations doc ver
    | ver == currentVersion = pure doc
    | ver > currentVersion = "Cannot read docs of version: " ++ show ver & fail
    | otherwise =
        do
            putStrLn $ "Migrating version " ++ show ver ++ " -> " ++ show (ver + 1)
            newDoc <- (versionMigrations !! ver) doc & toIO
            applyMigrations newDoc (ver + 1)

migrateAsNeeded :: Aeson.Value -> IO Aeson.Value
migrateAsNeeded doc =
    getVersion doc & toIO >>= applyMigrations doc
