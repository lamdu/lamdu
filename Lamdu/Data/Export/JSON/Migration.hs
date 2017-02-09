-- | Migrate any older version to current
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Data.Export.JSON.Migration (migrateAsNeeded) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import           Data.Text (unpack)
import qualified Lamdu.Data.Export.JSON.Migration.ToVersion1 as ToVersion1

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

migrateAsNeeded :: Aeson.Value -> IO Aeson.Value
migrateAsNeeded doc =
    do
        ver <- getVersion doc & toIO
        case ver of
            0 -> do
                putStrLn "Migrating version 0 -> 1"
                ToVersion1.migrate doc & toIO
            1 -> return doc
            _ -> "Cannot read docs of version: " ++ show ver & fail
    where
        toIO = either (fail . unpack) return
