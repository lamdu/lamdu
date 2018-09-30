module Lamdu.Data.Db
    ( withDB
    ) where

import           Control.Exception (onException)
import qualified Lamdu.Data.Db.Init as DbInit
import           Lamdu.Data.Db.Layout (DbM(..))
import           Lamdu.Data.Db.Migration (migration)
import           Lamdu.Data.Export.JSON (fileImportAll)
import qualified Lamdu.Paths as Paths
import qualified Revision.Deltum.Db as Db
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

initFreshDb :: Transaction.Store DbM -> IO ()
initFreshDb db =
    Paths.getDataFileName "freshdb.json"
    >>= fileImportAll >>= DbInit.initDb db

withDB :: FilePath -> (Transaction.Store DbM -> IO a) -> IO a
withDB lamduDir body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        alreadyExist <- Directory.doesDirectoryExist dbPath
        Db.withDB dbPath Db.defaultOptions $
            \ioDb ->
            do
                let db = Transaction.onStoreM DbM ioDb
                if alreadyExist
                    then migration db
                    else initFreshDb db `onException` Directory.removeDirectoryRecursive dbPath
                body db
    where
        dbPath = lamduDir </> "codeedit.db"
