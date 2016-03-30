module Data.Store.Db
    ( Db, withDb
    , lookup
    , transaction
    , store
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Utils (strictifyBS)
import           Data.Store.Transaction (Store(..))
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Database.LevelDB.Base (DB)
import qualified Database.LevelDB.Base as DB
import           Prelude hiding (lookup)
import           System.Random (randomIO)

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

lookup :: Db -> UUID -> IO (Maybe ByteString)
lookup db = DB.get db DB.defaultReadOptions . strictifyBS . UUID.toByteString

transaction :: Db -> [(UUID, Maybe ByteString)] -> IO ()
transaction db =
    DB.write db DB.defaultWriteOptions . map batchOp
    where
        batchOp (key, Nothing) = (DB.Del . strictifyBS . UUID.toByteString) key
        batchOp (key, Just value) = (DB.Put . strictifyBS . UUID.toByteString) key value

store :: Db -> Store IO
store db =
    Store
    { storeNewKey = randomIO
    , storeLookup = lookup db
    , storeAtomicWrite = transaction db
    }
