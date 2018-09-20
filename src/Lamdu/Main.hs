{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}
module Main
    ( main
    ) where

import qualified Control.Exception as E
import           Control.Monad (replicateM_)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           GHC.Stack (whoCreated)
import qualified Lamdu.Data.Db as Db
import           Lamdu.Data.Db.Layout (DbM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Export.JSON as Export
import qualified Lamdu.Editor as Editor
import qualified Lamdu.Opts as Opts
import qualified Lamdu.Paths as LamduPaths
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Directory as Directory
import           System.IO (hPutStrLn, stderr)

import           Lamdu.Prelude

main :: HasCallStack => IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        Opts.Parsed{_pLamduDB,_pCommand} <- Opts.get
        lamduDir <- maybe LamduPaths.getLamduDir pure _pLamduDB
        let withDB = Db.withDB lamduDir
        case _pCommand of
            Opts.DeleteDb -> deleteDB lamduDir
            Opts.Undo n -> withDB (undoN n)
            Opts.Import path -> withDB (importPath path)
            Opts.Export path -> withDB (exportToPath path)
            Opts.Editor opts -> withDB (Editor.run opts)
    `E.catch` \e@E.SomeException{} -> do
    hPutStrLn stderr $ "Main exiting due to exception: " ++ show e
    whoCreated e >>= traverse_ (hPutStrLn stderr)

deleteDB :: FilePath -> IO ()
deleteDB lamduDir =
    do
        putStrLn "Deleting DB..."
        Directory.removeDirectoryRecursive lamduDir

undoN :: Int -> Transaction.Store DbM -> IO ()
undoN n db =
    do
        putStrLn $ "Undoing " ++ show n ++ " times"
        DbLayout.runDbTransaction db $ replicateM_ n undo
    where
        undo =
            do
                actions <- VersionControl.makeActions
                fromMaybe (fail "Cannot undo any further") $ mUndo actions

importPath :: FilePath -> Transaction.Store DbM -> IO ()
importPath path db =
    Export.fileImportAll path
    <&> VersionControl.runAction
    >>= DbLayout.runDbTransaction db

exportToPath :: FilePath -> Transaction.Store DbM -> IO ()
exportToPath path db =
    Export.fileExportAll path
    & VersionControl.runAction
    & DbLayout.runDbTransaction db
    & join
