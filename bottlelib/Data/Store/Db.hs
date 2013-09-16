module Data.Store.Db
    (Db, withDb, lookup, transaction, store)
where

import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Store(..))
import Database.Sophia (Db)
import Prelude hiding (lookup)
import qualified Data.Store.Guid as Guid
import qualified Database.Sophia as Sophia

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb filePath act =
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation filePath
    Sophia.withDb env act

lookup :: Db -> Guid -> IO (Maybe ByteString)
lookup db = Sophia.getValue db . Guid.bs

transaction :: Db -> [(Guid, Maybe ByteString)] -> IO ()
transaction db changes =
  traverse_ applyChange changes
  where
    applyChange (key, Nothing) = Sophia.delValue db (Guid.bs key)
    applyChange (key, Just value) = Sophia.setValue db (Guid.bs key) value

-- You get a Store tagged however you like...
store :: Db -> Store IO
store db = Store {
  storeNewKey = Guid.new,
  storeLookup = lookup db,
  storeAtomicWrite = transaction db
  }
