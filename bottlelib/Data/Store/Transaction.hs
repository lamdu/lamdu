{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Store.Transaction
  ( Transaction, run, Property
  , Store(..)
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

import           Control.Applicative              (Applicative)
import           Control.Monad                    (liftM)
import           Control.Monad.Trans.Class        (MonadTrans(..))
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.State        (StateT, runStateT, get, gets, modify)
import           Data.Binary                      (Binary)
import           Data.Binary.Utils                (encodeS, decodeS)
import           Data.ByteString                  (ByteString)
import           Data.Map                         (Map)
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Monoid                      (mempty)
import           Data.Store.Guid                  (Guid)
import           Data.Store.IRef                  (IRef)
import           Data.Store.Rev.Change            (Key, Value)
import           Prelude                          hiding (lookup)
import qualified Data.Map                         as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef                  as IRef
import qualified Data.Store.Property              as Property

type Property t m = Property.Property (Transaction t m)

type Changes = Map Key (Maybe Value)

-- 't' is a phantom-type tag meant to make sure you run Transactions
-- with the right store
data Store t m = Store {
  storeNewKey :: m Key,
  storeLookup :: Key -> m (Maybe Value),
  storeAtomicWrite :: [(Key, Maybe Value)] -> m ()
  }

-- Define transformer stack:
newtype Transaction t m a = Transaction {
  unTransaction :: ReaderT (Store t m) (StateT Changes m) a
  } deriving (Monad, Applicative, Functor)
liftReaderT :: ReaderT (Store t m) (StateT Changes m) a -> Transaction t m a
liftReaderT = Transaction
liftStateT :: Monad m => StateT Changes m a -> Transaction t m a
liftStateT = liftReaderT . lift
liftInner :: Monad m => m a -> Transaction t m a
liftInner = Transaction . lift . lift

isEmpty :: Monad m => Transaction t m Bool
isEmpty = liftStateT (gets Map.null)

lookupBS :: Monad m => Guid -> Transaction t m (Maybe Value)
lookupBS guid = do
  changes <- liftStateT get
  case Map.lookup guid changes of
    Nothing -> do
      store <- liftReaderT ask
      liftInner $ storeLookup store guid
    Just res -> return res

insertBS :: Monad m => Guid -> ByteString -> Transaction t m ()
insertBS key = liftStateT . modify . Map.insert key . Just

delete :: Monad m => Guid -> Transaction t m ()
delete key = liftStateT . modify . Map.insert key $ Nothing

lookup :: (Monad m, Binary a) => Guid -> Transaction t m (Maybe a)
lookup = (liftM . fmap) decodeS . lookupBS

insert :: (Monad m, Binary a) => Guid -> a -> Transaction t m ()
insert key = insertBS key . encodeS

writeGuid :: (Monad m, Binary a) => Guid -> a -> Transaction t m ()
writeGuid = insert

guidExists :: Monad m => Guid -> Transaction t m Bool
guidExists = liftM isJust . lookupBS

readGuidMb :: (Monad m, Binary a) => Transaction t m a -> Guid -> Transaction t m a
readGuidMb nothingCase guid =
  maybe nothingCase return =<< lookup guid

readGuidDef :: (Monad m, Binary a) => a -> Guid -> Transaction t m a
readGuidDef = readGuidMb . return

readGuid :: (Monad m, Binary a) => Guid -> Transaction t m a
readGuid guid = readGuidMb failure guid
  where
    failure = fail $ show guid ++ " to inexistent object dereferenced"

deleteIRef :: Monad m => IRef a -> Transaction t m ()
deleteIRef = delete . IRef.guid

readIRefDef :: (Monad m, Binary a) => a -> IRef a -> Transaction t m a
readIRefDef def = readGuidDef def . IRef.guid

readIRef :: (Monad m, Binary a) => IRef a -> Transaction t m a
readIRef = readGuid . IRef.guid

irefExists :: (Monad m, Binary a) => IRef a -> Transaction t m Bool
irefExists = guidExists . IRef.guid

writeIRef :: (Monad m, Binary a) => IRef a -> a -> Transaction t m ()
writeIRef = writeGuid . IRef.guid

fromIRef :: (Monad m, Binary a) => IRef a -> Transaction t m (Property t m a)
fromIRef iref = liftM (flip Property.Property (writeIRef iref)) $ readIRef iref

fromIRefDef :: (Monad m, Binary a) => IRef a -> a -> Transaction t m (Property t m a)
fromIRefDef iref def = liftM (flip Property.Property (writeIRef iref)) $ readIRefDef def iref

newKey :: Monad m => Transaction t m Key
newKey = liftInner . storeNewKey =<< liftReaderT ask

newIRef :: (Monad m, Binary a) => a -> Transaction t m (IRef a)
newIRef val = do
  newGuid <- newKey
  insert newGuid val
  return $ IRef.unsafeFromGuid newGuid

newIRefWithGuid ::
  (Binary a, Monad m) =>
  (Guid -> Transaction t m (a, b)) -> Transaction t m (IRef a, b)
newIRefWithGuid f = do
  newGuid <- newKey
  let iref = IRef.unsafeFromGuid newGuid
  (val, extra) <- f newGuid
  writeIRef iref val
  return (iref, extra)

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
followBy :: (Monad m, Binary a) =>
            (b -> IRef a) ->
            Property t m b ->
            Transaction t m (Property t m a)
followBy conv = fromIRef . conv . Property.value

anchorRef :: (Monad m, Binary a) => String -> Transaction t m (Property t m a)
anchorRef = fromIRef . IRef.anchor

anchorRefDef :: (Monad m, Binary a) => String -> a -> Transaction t m (Property t m a)
anchorRefDef name def = flip fromIRefDef def $ IRef.anchor name

run :: Monad m => Store t m -> Transaction t m a -> m a
run store transaction = do
  (res, changes) <- (`runStateT` mempty) . (`runReaderT` store) . unTransaction $ transaction
  storeAtomicWrite store $ Map.toList changes
  return res

assocDataRef ::
  (Binary a, Monad m) =>
  ByteString -> Guid -> Transaction t m (Property t m (Maybe a))
assocDataRef str guid = do
  val <- lookup assocGuid
  return $ Property.Property val set
  where
    assocGuid = Guid.combine guid $ Guid.make str
    set Nothing = delete assocGuid
    set (Just x) = writeGuid assocGuid x

assocDataRefDef ::
  (Eq a, Binary a, Monad m) =>
  a -> ByteString -> Guid -> Transaction t m (Property t m a)
assocDataRefDef def str =
  liftM (Property.pureCompose (fromMaybe def) f) . assocDataRef str
  where
    f x
      | x == def = Nothing
      | otherwise = Just x
