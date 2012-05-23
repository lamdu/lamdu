module Data.Store.Db
    (Db, withDb,
     nextKeyBS, nextKey, lookup, transaction, store)
where

import           Control.Arrow          (second)
import           Control.Exception      (bracket)
import           Prelude                hiding (lookup)
import qualified Database.Berkeley.Db   as Berkeley
import           Data.ByteString        (ByteString)
import           Data.Binary            (Binary)
import           Data.Binary.Utils      (decodeS)
import           Data.Store.Guid        (Guid)
import qualified Data.Store.Guid        as Guid
import           Data.Store.Transaction (Store(..))
import           System.Directory       (createDirectoryIfMissing)

data Db = Db {
  dbBerkeley :: Berkeley.Db,
  _dbEnv :: Berkeley.DbEnv
  }

type Cursor = Berkeley.DbCursor

open :: FilePath -> IO Db
open fileName = do
  createDirectoryIfMissing False envDir
  env <- Berkeley.dbEnv_create []
  Berkeley.dbEnv_open [Berkeley.DB_CREATE, Berkeley.DB_INIT_MPOOL,
                       -- Berkeley.DB_INIT_TXN,
                       Berkeley.DB_INIT_LOCK,
                       Berkeley.DB_INIT_LOG] 0 env envDir
  db <- Berkeley.db_create [] env
--  Berkeley.dbEnv_withTxn [] [] env Nothing $ \txn ->
  Berkeley.db_open [Berkeley.DB_CREATE] Berkeley.DB_BTREE 0 db
              Nothing --(Just txn)
              fileName (Just "DB title")
  return $ Db db env
  where
    envDir = fileName ++ ".env"

close :: Db -> IO ()
close (Db db env) = do
  Berkeley.db_close [] db
  -- TODO: waitForEmptyMap m
  Berkeley.dbEnv_close [] env

nextKeyBS :: Cursor -> IO (Maybe (ByteString, ByteString))
nextKeyBS = Berkeley.dbCursor_get [Berkeley.DB_NEXT]

nextKey :: Binary a => Cursor -> IO (Maybe (ByteString, a))
nextKey cursor = (fmap . fmap . second) decodeS (nextKeyBS cursor)

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb filePath = bracket (open filePath) close

lookup :: Db -> Guid -> IO (Maybe ByteString)
lookup db = Berkeley.db_get [] (dbBerkeley db) Nothing . Guid.bs

transaction :: Db -> [(Guid, Maybe ByteString)] -> IO ()
transaction db = -- . Berkeley.dbEnv_withTxn [] [] (dbEnv db) Nothing $ \txn ->
  mapM_ (uncurry (applyChange
                  -- (Just txn)
                  Nothing
                 ))
  where
    applyChange txn key Nothing = Berkeley.db_del [] (dbBerkeley db) txn (Guid.bs key)
    applyChange txn key (Just value) = Berkeley.db_put [] (dbBerkeley db) txn (Guid.bs key) value

-- You get a Store tagged however you like...
store :: Db -> Store t IO
store db = Store {
  storeNewKey = Guid.new,
  storeLookup = lookup db,
  storeAtomicWrite = transaction db
  }
