module Lamdu.Data.Db
    ( withDB, ImplicitFreshDb(..)
    ) where

import           Control.Exception (onException)
import qualified Lamdu.Data.Db.Init as DbInit
import           Lamdu.Data.Db.Layout (DbM(..), ViewM, curDbSchemaVersion)
import           Lamdu.Data.Export.JSON.Import (fileImportAll)
import qualified Lamdu.Paths as Paths
import qualified Revision.Deltum.Db as Db
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

type T = Transaction

data ImplicitFreshDb = ImplicitFreshDb | NoImplicitFreshDb | FailIfFresh String
    deriving (Eq, Show)

importFreshDb :: ImplicitFreshDb -> IO (T ViewM ())
importFreshDb NoImplicitFreshDb = pure (pure ())
importFreshDb (FailIfFresh msg) = fail msg
importFreshDb ImplicitFreshDb =
    Paths.getDataFileName "freshdb.json" >>= fileImportAll <&> snd

withDB :: FilePath -> ImplicitFreshDb -> (Transaction.Store DbM -> IO a) -> IO a
withDB lamduDir implicitFreshDb body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        alreadyExist <- Directory.doesDirectoryExist dbPath
        let options =
                Db.defaultOptions
                { Db.createIfMissing = not alreadyExist
                , Db.errorIfExists = not alreadyExist
                }
        Db.withDB dbPath options $
            \ioDb ->
            do
                let db = Transaction.onStoreM DbM ioDb
                let dbInit = importFreshDb implicitFreshDb >>= DbInit.initDb db
                dbInit `onException` Directory.removeDirectoryRecursive dbPath
                    & unless alreadyExist
                body db
    where
        dbPath = lamduDir </> "schema-" <> show curDbSchemaVersion <> ".db"
