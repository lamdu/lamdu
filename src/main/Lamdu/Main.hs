module Main
    ( main
    ) where

import qualified Control.Exception as E
import           Control.Monad (replicateM_)
import           Data.Either (fromRight)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           GHC.Stack (whoCreated)
import qualified Lamdu.Data.Db as Db
import           Lamdu.Data.Db.Layout (DbM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Export.JSON as Export
import qualified Lamdu.Data.Export.JSON.Import as Import
import qualified Lamdu.Editor as Editor
import qualified Lamdu.Opts as Opts
import qualified Lamdu.Paths as LamduPaths
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Directory as Directory
import           System.Exit (ExitCode(..))
import           System.IO (hPutStrLn, stderr)

import           Lamdu.Prelude

main :: HasCallStack => IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        Opts.ParsedCommand cmd <- Opts.get
        lamduDir <- maybe LamduPaths.getLamduDir pure (cmd ^. Opts.cLamduDB)
        let withDB = Db.withDB lamduDir
        case cmd ^. Opts.cCommand of
            Opts.DeleteDb -> deleteDB lamduDir
            Opts.Undo n ->
                withDB (Db.FailIfFresh "not creating DB for undo") (undoN n)
            Opts.Import opts ->
                withDB implicitFreshDb (importPath (opts ^. Opts.importPath))
                where
                    implicitFreshDb
                        | opts ^. Opts.importImplicitPrelude = Db.ImplicitFreshDb
                        | otherwise = Db.NoImplicitFreshDb
            Opts.Export path ->
                withDB (Db.FailIfFresh "not creating DB for export")
                (exportToPath path)
            Opts.Editor opts -> withDB Db.ImplicitFreshDb (Editor.run opts)
        `E.catch` \e@E.SomeException{} ->
        case E.fromException e of
        Just ex -> E.throwIO (ex::ExitCode)
        Nothing ->
            do
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
                fromMaybe (error "Cannot undo any further") $ mUndo actions

importPath :: FilePath -> Transaction.Store DbM -> IO ()
importPath path db =
    Import.fileImportAll path
    <&> VersionControl.runAction . snd
    >>= DbLayout.runDbTransaction db

exportToPath :: FilePath -> Transaction.Store DbM -> IO ()
exportToPath path db =
    Export.fileExportAll path
    & VersionControl.runAction
    & DbLayout.runDbTransaction db
    >>= fromRight (putStrLn "Uses deleted definition")
