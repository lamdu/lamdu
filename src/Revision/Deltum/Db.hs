module Revision.Deltum.Db
    ( withDB, DB.defaultOptions, DB.Options(..)
    ) where

import           Control.Exception (bracket)
import qualified Data.ByteString.Extended as BS
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Database.RocksDB.Base (DB)
import qualified Database.RocksDB.Base as DB
import           Revision.Deltum.Transaction (Store(..))
import           System.Random (randomIO)

import           Lamdu.Prelude hiding (lookup)

lookup :: DB -> UUID -> IO (Maybe ByteString)
lookup db = DB.get db DB.defaultReadOptions . BS.strictify . UUID.toByteString

transaction :: DB -> [(UUID, Maybe ByteString)] -> IO ()
transaction db =
    DB.write db DB.defaultWriteOptions . map batchOp
    where
        batchOp (key, Nothing) = (DB.Del . BS.strictify . UUID.toByteString) key
        batchOp (key, Just value) = (DB.Put . BS.strictify . UUID.toByteString) key value

store :: DB -> Store IO
store db =
    Store
    { storeNewKey = randomIO
    , storeLookup = lookup db
    , storeAtomicWrite = transaction db
    }

withDB :: FilePath -> DB.Options -> (Store IO -> IO a) -> IO a
withDB path opts act = bracket (DB.open path opts) DB.close (act . store)
