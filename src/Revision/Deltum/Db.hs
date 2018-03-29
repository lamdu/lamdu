module Revision.Deltum.Db
    ( withDB, DB.defaultOptions, DB.Options(..)
    ) where

import           Data.ByteString.Utils (strictifyBS)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Database.LevelDB.Base (DB)
import qualified Database.LevelDB.Base as DB
import           Revision.Deltum.Transaction (Store(..))
import           System.Random (randomIO)

import           Lamdu.Prelude hiding (lookup)

lookup :: DB -> UUID -> IO (Maybe ByteString)
lookup db = DB.get db DB.defaultReadOptions . strictifyBS . UUID.toByteString

transaction :: DB -> [(UUID, Maybe ByteString)] -> IO ()
transaction db =
    DB.write db DB.defaultWriteOptions . map batchOp
    where
        batchOp (key, Nothing) = (DB.Del . strictifyBS . UUID.toByteString) key
        batchOp (key, Just value) = (DB.Put . strictifyBS . UUID.toByteString) key value

store :: DB -> Store IO
store db =
    Store
    { storeNewKey = randomIO
    , storeLookup = lookup db
    , storeAtomicWrite = transaction db
    }

withDB :: FilePath -> DB.Options -> (Store IO -> IO a) -> IO a
withDB path opts act = DB.withDB path opts (act . store)
