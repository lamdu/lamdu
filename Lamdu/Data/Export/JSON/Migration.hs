-- | Migrate any older version to current
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Data.Export.JSON.Migration (migrateAsNeeded) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Text (unpack)
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion1 as ToVersion1
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion2 as ToVersion2

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
    [ToVersion1.migrate, ToVersion2.migrate]

currentVersion :: Int
currentVersion = length versionMigrations

migrateAsNeeded :: Aeson.Value -> IO Aeson.Value
migrateAsNeeded doc =
    do
        ver <- getVersion doc & toIO
        if ver == currentVersion
            then return doc
            else
                if ver > currentVersion
                then "Cannot read docs of version: " ++ show ver & fail
                else (versionMigrations !! ver) doc & toIO
    where
        toIO = either (fail . unpack) return
