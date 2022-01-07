module Test.Lamdu.Db
    ( ramDB
    ) where

import qualified Control.Lens as Lens
import           Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import qualified Data.Map as Map
import           Data.UUID.Types (UUID)
import qualified Lamdu.Data.Db.Init as DbInit
import           Lamdu.Data.Db.Layout (DbM(..))
import           Lamdu.Data.Export.JSON (fileImportAll)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Random (randomIO)

import           Test.Lamdu.Prelude

initFreshDb :: [FilePath] -> Transaction.Store DbM -> IO ()
initFreshDb paths db = traverse fileImportAll paths <&> (^. traverse . Lens._2) >>= DbInit.initDb db

-- | Make an action to efficiently generate a fresh RAM DB
ramDB :: [FilePath] -> IO (IO (Transaction.Store DbM))
ramDB paths =
    do
        origDb <- newIORef Map.empty
        let store :: IORef (Map UUID ByteString) -> Transaction.Store DbM
            store db =
                Transaction.onStoreM DbM Transaction.Store
                { Transaction.storeNewKey = randomIO
                , Transaction.storeLookup = \key -> readIORef db <&> (^. Lens.at key)
                , Transaction.storeAtomicWrite =
                    \updates ->
                    updates <&> updateKey & foldr (.) id & modifyIORef db
                }
        initFreshDb paths (store origDb)
        readIORef origDb >>= newIORef <&> store
            & pure
    where
        updateKey (k, v) = Lens.at k .~ v
