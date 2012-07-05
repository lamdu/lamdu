module Data.Store.Db
    (Db, withDb, lookup, transaction, store)
where

import qualified Control.Exception      as Exc
import           Prelude                hiding (lookup)
import qualified Database.KeyValueHash  as HashDB
import           Data.ByteString        (ByteString)
import           Data.Store.Guid        (Guid)
import qualified Data.Store.Guid        as Guid
import           Data.Store.Transaction (Store(..))

type Db = HashDB.Database

-- TODO: this should be automatic in HashDB
hashSize :: HashDB.Size
hashSize = HashDB.mkSize $ 2 ^ (17::Int)

open :: FilePath -> IO Db
open fileName =
  HashDB.openDatabase fileName HashDB.stdHash hashSize
  `Exc.catch`
  (\(Exc.SomeException _) ->
    HashDB.createDatabase fileName HashDB.stdHash hashSize)

close :: Db -> IO ()
close _ = return ()

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb filePath = Exc.bracket (open filePath) close

lookup :: Db -> Guid -> IO (Maybe ByteString)
lookup db = HashDB.readKey db . Guid.bs

transaction :: Db -> [(Guid, Maybe ByteString)] -> IO ()
transaction db = mapM_ applyChange
  where
    applyChange (key, Nothing) = HashDB.deleteKey db (Guid.bs key)
    applyChange (key, Just value) = HashDB.writeKey db (Guid.bs key) value

-- You get a Store tagged however you like...
store :: Db -> Store t IO
store db = Store {
  storeNewKey = Guid.new,
  storeLookup = lookup db,
  storeAtomicWrite = transaction db
  }
