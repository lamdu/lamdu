-- | Check for DB version consistency and either migrate to current version
-- or present instructions on how to do so.
--
-- Schema versions:
-- * 0 (or none): any db schemas prior to defining the schema versions.
-- * 1: The schema version at 2018.02.05, like version 0 but verified.

module Lamdu.Data.Db.Migration
    ( migration
    ) where

import           Control.Exception (try, SomeException(..))
import qualified GUI.Momentu as M
import           Lamdu.Data.Db.Layout (runDbTransaction, dbSchemaVersion, curDbSchemaVersion, guiState)
import           Lamdu.Data.Export.JSON (verifyAll)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.VersionControl as VersionControl
import           Revision.Deltum.Db (DB)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

migrateFromNoVersion :: DB -> IO ()
migrateFromNoVersion db =
    do
        putStrLn "No DB version found (pre 2018.02.03), adding version"
        do
            updateMissingCursor
            Transaction.writeIRef dbSchemaVersion 0
            & runDbTransaction db
        migrateFromVersion db 0
    where
        -- | In commit 567ae4620f9222889a76a84936b9b09432a29698, the cursor
        -- became a DB field. Easy to migrate from pre-cursor DB so do so:
        updateMissingCursor =
            do
                exists <- Transaction.irefExists guiState
                Transaction.writeIRef guiState (M.GUIState WidgetIds.defaultCursor mempty)
                    & unless exists

showIncompatibleDbMessage :: IO ()
showIncompatibleDbMessage =
    do
        concat
            [ "Your Lamdu DB has been created by an old version of Lamdu "
            , "which the current version is incompatible with.\n"
            , "You can migrate your DB by following the instructions in doc/DbMigration.md, "
            , "or if clearing the data is an option simply just run \"lamdu deletedb\".\n"
            ]
            & putStrLn
        fail "incompatible db"

migrateFromVersion :: DB -> Int -> IO ()
migrateFromVersion _ ver | ver > curDbSchemaVersion = fail "DB from newer Lamdu version!"
migrateFromVersion _ ver | ver == curDbSchemaVersion = pure ()
migrateFromVersion db 0 =
    VersionControl.runAction verifyAll
    & runDbTransaction db
    & try
    >>=
    \case
    Left SomeException{} -> showIncompatibleDbMessage
    Right{} ->
        do
            putStrLn "Migrating DB schema to version 1"
            Transaction.writeIRef dbSchemaVersion 1 & runDbTransaction db
            migrateFromVersion db 1
migrateFromVersion _ ver | ver < curDbSchemaVersion = showIncompatibleDbMessage
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
