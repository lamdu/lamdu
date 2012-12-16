{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Data.Store.Transaction
  ( Transaction, run, Property
  , Store(..), onStoreM
  , lookupBS, lookup
  , insertBS, insert
  , delete, deleteIRef
  , readIRef, readIRefDef, writeIRef
  , readGuid, readGuidDef, writeGuid
  , isEmpty
  , guidExists, irefExists
  , newIRef, newKey, newIRefWithGuid
  , fromIRef, fromIRefDef
  , followBy
  , anchorRef, anchorRefDef
  , assocDataRef, assocDataRefDef
  )
where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, runStateT, get, gets, modify)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Binary.Utils (encodeS, decodeS)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Rev.Change (Key, Value)
import Prelude                          hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property

type Property m = Property.Property (Transaction m)

type Changes = Map Key (Maybe Value)

-- 't' is a phantom-type tag meant to make sure you run Transactions
-- with the right store
data Store m = Store
  { storeNewKey :: m Key
  , storeLookup :: Key -> m (Maybe Value)
  , storeAtomicWrite :: [(Key, Maybe Value)] -> m ()
  }

onStoreM :: (forall a. m a -> n a) -> Store m -> Store n
onStoreM f x = x
  { storeNewKey = f $ storeNewKey x
  , storeLookup = f . storeLookup x
  , storeAtomicWrite = f . storeAtomicWrite x
  }

-- Define transformer stack:
newtype Transaction m a = Transaction {
  unTransaction :: ReaderT (Store m) (StateT Changes m) a
  } deriving (Monad, Applicative, Functor)
liftReaderT :: ReaderT (Store m) (StateT Changes m) a -> Transaction m a
liftReaderT = Transaction
liftStateT :: MonadA m => StateT Changes m a -> Transaction m a
liftStateT = liftReaderT . lift
liftInner :: MonadA m => m a -> Transaction m a
liftInner = Transaction . lift . lift

isEmpty :: MonadA m => Transaction m Bool
isEmpty = liftStateT (gets Map.null)

lookupBS :: MonadA m => Guid -> Transaction m (Maybe Value)
lookupBS guid = do
  changes <- liftStateT get
  case Map.lookup guid changes of
    Nothing -> do
      store <- liftReaderT ask
      liftInner $ storeLookup store guid
    Just res -> return res

insertBS :: MonadA m => Guid -> ByteString -> Transaction m ()
insertBS key = liftStateT . modify . Map.insert key . Just

delete :: MonadA m => Guid -> Transaction m ()
delete key = liftStateT . modify . Map.insert key $ Nothing

lookup :: (MonadA m, Binary a) => Guid -> Transaction m (Maybe a)
lookup = (fmap . fmap) decodeS . lookupBS

insert :: (MonadA m, Binary a) => Guid -> a -> Transaction m ()
insert key = insertBS key . encodeS

writeGuid :: (MonadA m, Binary a) => Guid -> a -> Transaction m ()
writeGuid = insert

guidExists :: MonadA m => Guid -> Transaction m Bool
guidExists = fmap isJust . lookupBS

readGuidMb :: (MonadA m, Binary a) => Transaction m a -> Guid -> Transaction m a
readGuidMb nothingCase guid =
  maybe nothingCase return =<< lookup guid

readGuidDef :: (MonadA m, Binary a) => a -> Guid -> Transaction m a
readGuidDef = readGuidMb . return

readGuid :: (MonadA m, Binary a) => Guid -> Transaction m a
readGuid guid = readGuidMb failure guid
  where
    failure = fail $ "Inexistent guid: " ++ show guid ++ " referenced"

deleteIRef :: MonadA m => IRef (Tag m) a -> Transaction m ()
deleteIRef = delete . IRef.guid

readIRefDef :: (MonadA m, Binary a) => a -> IRef (Tag m) a -> Transaction m a
readIRefDef def = readGuidDef def . IRef.guid

readIRef :: (MonadA m, Binary a) => IRef (Tag m) a -> Transaction m a
readIRef = readGuid . IRef.guid

irefExists :: (MonadA m, Binary a) => IRef (Tag m) a -> Transaction m Bool
irefExists = guidExists . IRef.guid

writeIRef :: (MonadA m, Binary a) => IRef (Tag m) a -> a -> Transaction m ()
writeIRef = writeGuid . IRef.guid

fromIRef :: (MonadA m, Binary a) => IRef (Tag m) a -> Transaction m (Property m a)
fromIRef iref = fmap (flip Property.Property (writeIRef iref)) $ readIRef iref

fromIRefDef :: (MonadA m, Binary a) => IRef (Tag m) a -> a -> Transaction m (Property m a)
fromIRefDef iref def = fmap (flip Property.Property (writeIRef iref)) $ readIRefDef def iref

newKey :: MonadA m => Transaction m Key
newKey = liftInner . storeNewKey =<< liftReaderT ask

newIRef :: (MonadA m, Binary a) => a -> Transaction m (IRef (Tag m) a)
newIRef val = do
  newGuid <- newKey
  insert newGuid val
  return $ IRef.unsafeFromGuid newGuid

newIRefWithGuid ::
  (Binary a, MonadA m) =>
  (Guid -> Transaction m (a, b)) -> Transaction m (IRef (Tag m) a, b)
newIRefWithGuid f = do
  newGuid <- newKey
  let iref = IRef.unsafeFromGuid newGuid
  (val, extra) <- f newGuid
  writeIRef iref val
  return (iref, extra)

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
followBy :: (MonadA m, Binary a) =>
            (b -> IRef (Tag m) a) ->
            Property m b ->
            Transaction m (Property m a)
followBy conv = fromIRef . conv . Property.value

anchorRef :: (MonadA m, Binary a) => String -> Transaction m (Property m a)
anchorRef = fromIRef . IRef.anchor

anchorRefDef :: (MonadA m, Binary a) => String -> a -> Transaction m (Property m a)
anchorRefDef name def = flip fromIRefDef def $ IRef.anchor name

run :: MonadA m => Store m -> Transaction m a -> m a
run store transaction = do
  (res, changes) <- (`runStateT` mempty) . (`runReaderT` store) . unTransaction $ transaction
  storeAtomicWrite store $ Map.toList changes
  return res

assocDataRef ::
  (Binary a, MonadA m) =>
  ByteString -> Guid -> Transaction m (Property m (Maybe a))
assocDataRef str guid = do
  val <- lookup assocGuid
  return $ Property.Property val set
  where
    assocGuid = Guid.combine guid $ Guid.make str
    set Nothing = delete assocGuid
    set (Just x) = writeGuid assocGuid x

assocDataRefDef ::
  (Eq a, Binary a, MonadA m) =>
  a -> ByteString -> Guid -> Transaction m (Property m a)
assocDataRefDef def str =
  fmap (Property.pureCompose (fromMaybe def) f) . assocDataRef str
  where
    f x
      | x == def = Nothing
      | otherwise = Just x
