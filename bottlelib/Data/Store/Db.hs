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
import qualified Data.Map               as Map
import           Data.Map               (Map)
import           Data.IORef

data Db = Db
  { dbKeyValueStore :: HashDB.Database
  , dbCache :: IORef (Map HashDB.Key (Maybe HashDB.Value))
  }

-- TODO: this should be automatic in HashDB
hashSize :: HashDB.Size
hashSize = HashDB.mkSize $ 2 ^ (17::Int)

open :: FilePath -> IO Db
open fileName = do
  db <-
    HashDB.openDatabase fileName HashDB.stdHash hashSize
    `Exc.catch`
    (\(Exc.SomeException _) ->
      HashDB.createDatabase fileName HashDB.stdHash hashSize)
  cacheRef <- newIORef Map.empty
  return $ Db db cacheRef

close :: Db -> IO ()
close _ = return ()

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb filePath = Exc.bracket (open filePath) close

modifyIORef_ :: IORef a -> (a -> a) -> IO ()
modifyIORef_ v f = atomicModifyIORef v (flip (,) () . f)

lookup :: Db -> Guid -> IO (Maybe ByteString)
lookup db guid = do
  cache <- readIORef $ dbCache db
  let inCache = Map.lookup bs cache
  case inCache of
    Nothing -> do
      result <- HashDB.readKey (dbKeyValueStore db) bs
      modifyIORef_ (dbCache db) $ Map.insert bs result
      return result
    Just result -> return result
  where
    bs = Guid.bs guid

transaction :: Db -> [(Guid, Maybe ByteString)] -> IO ()
transaction db = mapM_ modification
  where
    modification (key, mValue) = do
      modifyIORef_ (dbCache db) $ Map.insert bs mValue
      applyChange mValue
      where
        bs = Guid.bs key
        applyChange Nothing = HashDB.deleteKey (dbKeyValueStore db) bs
        applyChange (Just value) = HashDB.writeKey (dbKeyValueStore db) bs value

-- You get a Store tagged however you like...
store :: Db -> Store t IO
store db = Store {
  storeNewKey = Guid.new,
  storeLookup = lookup db,
  storeAtomicWrite = transaction db
  }
