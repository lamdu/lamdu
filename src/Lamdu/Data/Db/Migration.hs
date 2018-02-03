-- | Initialize a database, populating it with "freshdb.json" if needed
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Data.Db.Migration
    ( migration
    ) where

import           Data.Store.Db (DB)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Db.Layout (runDbTransaction, dbSchemaVersion, curDbSchemaVersion, guiState)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified GUI.Momentu as M

import           Lamdu.Prelude

migrateFromNoVersion :: DB -> IO ()
migrateFromNoVersion db =
    do
        putStrLn "No DB version found (pre 2018.02.03), adding version"
        do
            updateMissingCursor
            Transaction.writeIRef dbSchemaVersion 0
            & runDbTransaction db
    where
        -- | In commit 567ae4620f9222889a76a84936b9b09432a29698, the cursor
        -- became a DB field. Easy to migrate from pre-cursor DB so do so:
        updateMissingCursor =
            do
                exists <- Transaction.irefExists guiState
                Transaction.writeIRef guiState (M.GUIState WidgetIds.defaultCursor mempty)
                    & unless exists

migrateFromVersion :: DB -> Int -> IO ()
migrateFromVersion _ ver | ver > curDbSchemaVersion = fail "DB from newer Lamdu version!"
migrateFromVersion _ ver | ver == curDbSchemaVersion = return ()
migrateFromVersion _ ver = fail ("Unexpected Lamdu DB version: " ++ show ver)

migration :: DB -> IO ()
migration db =
    do
        schemaExists <- Transaction.irefExists dbSchemaVersion & runDbTransaction db
        if schemaExists
            then
                Transaction.readIRef dbSchemaVersion & runDbTransaction db
                >>= migrateFromVersion db
            else
                migrateFromNoVersion db
