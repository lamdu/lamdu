-- | Migrate any older version to current
module Lamdu.Data.Export.JSON.Migration
    ( migrateAsNeeded
    , currentVersion
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Text (unpack)
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion1 as ToVersion1
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion10 as ToVersion10
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion11 as ToVersion11
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion12 as ToVersion12
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion13 as ToVersion13
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion14 as ToVersion14
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion2 as ToVersion2
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion3 as ToVersion3
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion4 as ToVersion4
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion5 as ToVersion5
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion6 as ToVersion6
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion7 as ToVersion7
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion8 as ToVersion8
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion9 as ToVersion9

import           Lamdu.Prelude

getVersion :: Aeson.Value -> Either Text Codec.Version
getVersion (Aeson.Array values) =
    case values ^? Lens.ix 0 of
        Just (Aeson.Object obj) ->
            case obj ^. Lens.at "schemaVersion" of
            Nothing -> Right (Codec.Version 0)
            Just (Aeson.Number ver) -> Codec.Version (truncate ver) & Right
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
    , ToVersion9.migrate
    , ToVersion10.migrate
    , ToVersion11.migrate
    , ToVersion12.migrate
    , ToVersion13.migrate
    , ToVersion14.migrate
    ]

currentVersion :: Codec.Version
currentVersion = Codec.Version (length versionMigrations)

toIO :: Either Text a -> IO a
toIO = either (fail . unpack) pure

applyMigration :: Codec.Version -> Aeson.Value -> Either Text Aeson.Value
applyMigration (Codec.Version ver) = versionMigrations !! ver

applyMigrations :: Aeson.Value -> Codec.Version -> IO Aeson.Value
applyMigrations doc ver@(Codec.Version verNum)
    | ver == currentVersion = pure doc
    | ver > currentVersion =
        unwords
        [ "Cannot read JSON from version:"
        , show ver, "but only up to", show currentVersion ]
        & fail
    | otherwise =
        do
            let nextVer = Codec.Version (verNum + 1)
            putStrLn $ "Migrating version " ++ show ver ++ " -> " ++ show nextVer
            newDoc <- applyMigration ver doc & toIO
            applyMigrations newDoc nextVer

migrateAsNeeded :: Aeson.Value -> IO (Codec.Version, Aeson.Value)
migrateAsNeeded doc =
    do
        origVersion <- getVersion doc & toIO
        newDoc <- applyMigrations doc origVersion
        pure (origVersion, newDoc)
