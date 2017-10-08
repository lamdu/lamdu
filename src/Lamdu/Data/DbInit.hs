-- | Initialize a database, populating it with "freshdb.json" if needed
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Data.DbInit
    ( withDB
    ) where

import           Control.Exception (onException)
import           Data.Store.Db (Db)
import qualified Data.Store.Db as Db
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Export.JSON (fileImportAll)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

type T = Transaction

setName :: (Monad m, UniqueId.ToUUID a) => a -> Text -> T m ()
setName = Transaction.setP . DbLayout.assocNameRef

newBranch :: Monad m => Text -> Version m -> T m (Branch m)
newBranch name ver =
    do
        branch <- Branch.new ver
        setName (Branch.uuid branch) name
        return branch

initDb :: Db -> T DbLayout.ViewM () -> IO ()
initDb db importAct =
    DbLayout.runDbTransaction db $
    do
        emptyVersion <- Version.makeInitialVersion []
        master <- newBranch "master" emptyVersion
        view <- View.new master
        Transaction.writeIRef DbLayout.cursor WidgetIds.defaultCursor
        let writeRevAnchor f = Transaction.writeIRef (f DbLayout.revisionIRefs)
        writeRevAnchor DbLayout.view view
        writeRevAnchor DbLayout.branches [master]
        writeRevAnchor DbLayout.currentBranch master
        writeRevAnchor DbLayout.redos []
        let paneWId = WidgetIdIRef.fromIRef $ DbLayout.panes DbLayout.codeIRefs
        DbLayout.runViewTransaction view $
            do
                let writeCodeAnchor f = Transaction.writeIRef (f DbLayout.codeIRefs)
                writeCodeAnchor DbLayout.globals mempty
                writeCodeAnchor DbLayout.panes mempty
                writeCodeAnchor DbLayout.preJumps []
                writeCodeAnchor DbLayout.preCursor paneWId
                writeCodeAnchor DbLayout.postCursor paneWId
                writeCodeAnchor DbLayout.tids mempty
                writeCodeAnchor DbLayout.tags mempty
                importAct
        -- Prevent undo into the invalid empty revision
        newVer <- Branch.curVersion master
        Version.preventUndo newVer

-- | In commit 567ae4620f9222889a76a84936b9b09432a29698, the cursor
-- became a DB field. Easy to migrate from pre-cursor DB so do so:
updateMissingCursor :: T DbLayout.DbM ()
updateMissingCursor =
    do
        exists <- Transaction.irefExists DbLayout.cursor
        Transaction.writeIRef DbLayout.cursor WidgetIds.defaultCursor
            & unless exists

withDB :: FilePath -> (Db -> IO a) -> IO a
withDB lamduDir body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        alreadyExist <- Directory.doesDirectoryExist dbPath
        let options =
                Db.defaultOptions
                { Db.createIfMissing = not alreadyExist
                , Db.errorIfExists = not alreadyExist
                }
        Db.withDB dbPath options $ \db ->
            do
                DbLayout.runDbTransaction db updateMissingCursor
                Paths.get Paths_Lamdu.getDataFileName "freshdb.json"
                    >>= fileImportAll >>= initDb db
                    & (`onException` Directory.removeDirectoryRecursive dbPath)
                    & unless alreadyExist
                body db
    where
        dbPath = lamduDir </> "codeedit.db"
