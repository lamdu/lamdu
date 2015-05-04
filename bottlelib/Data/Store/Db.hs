module Data.Store.Db
    ( Db, withDb
    , lookup
    , transaction
    , store
    ) where

import           Data.ByteString (ByteString)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid
import           Data.Store.Transaction (Store(..))
import           Database.LevelDB.Base (DB)
import qualified Database.LevelDB.Base as DB
import           Prelude hiding (lookup)

-- TODO: Remove this
type Db = DB

options :: DB.Options
options =
    DB.defaultOptions
    { DB.createIfMissing = True
    , DB.errorIfExists = False
    }

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb filePath = DB.withDB filePath options

lookup :: Db -> Guid -> IO (Maybe ByteString)
lookup db = DB.get db DB.defaultReadOptions . Guid.bs

transaction :: Db -> [(Guid, Maybe ByteString)] -> IO ()
transaction db =
    DB.write db DB.defaultWriteOptions . map batchOp
    where
        batchOp (key, Nothing) = DB.Del (Guid.bs key)
        batchOp (key, Just value) = DB.Put (Guid.bs key) value

store :: Db -> Store IO
store db =
    Store
    { storeNewKey = Guid.new
    , storeLookup = lookup db
    , storeAtomicWrite = transaction db
    }
