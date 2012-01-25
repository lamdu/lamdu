{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Store.Transaction
    (Transaction, run, Property,
     Store(..),
     lookupBS, lookup,
     insertBS, insert,
     deleteBS, delete, deleteIRef,
     readIRef, readIRefDef, writeIRef,
     isEmpty,
     irefExists,
     newIRef, newContainerRef, newKey,
     fromIRef, fromIRefDef,
     followBy,
     anchorRef, anchorRefDef,
     Container, containerStr,
     fromContainerRef, fromContainerRefDef,
     anchorContainer, anchorContainerDef)
where

import           Prelude                          hiding (lookup)
import           Control.Applicative              (Applicative)
import           Control.Monad                    (liftM)
import           Control.Monad.Trans.State        (StateT, runStateT, get, gets, modify)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Class        (MonadTrans(..))
import           Data.ByteString.UTF8             (fromString)
import           Data.Binary                      (Binary)
import           Data.Binary.Utils                (encodeS, decodeS)
import           Data.Store.IRef                  (IRef)
import qualified Data.Store.IRef                  as IRef
import           Data.Store.ContainerRef          (ContainerRef)
import qualified Data.Store.ContainerRef          as ContainerRef
import           Data.Store.Guid                  (Guid)
import qualified Data.Store.Guid                  as Guid
import qualified Data.Store.Property              as Property
import           Data.Monoid                      (mempty)
import           Data.Maybe                       (isJust)
import           Data.ByteString                  (ByteString)
import qualified Data.Map                         as Map
import           Data.Map                         (Map)

type Property t m = Property.Property (Transaction t m)
type Container k t m a = k -> Property t m a

type Key = Guid
type Value = Maybe ByteString -- Nothing means delete, Just means insert/modify
type Changes = Map Key Value

-- 't' is a phantom-type tag meant to make sure you run Transactions
-- with the right store
data Store t m = Store {
  storeNewKey :: m Key,
  storeLookup :: Key -> m Value,
  storeAtomicWrite :: [(Key, Value)] -> m ()
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

lookupBS :: Monad m => Key -> Transaction t m Value
lookupBS guid = do
  changes <- liftStateT get
  case Map.lookup guid changes of
    Nothing -> do
      store <- liftReaderT ask
      liftInner $ storeLookup store guid
    Just res -> return res

insertBS :: Monad m => Key -> ByteString -> Transaction t m ()
insertBS key = liftStateT . modify . Map.insert key . Just

deleteBS :: Monad m => Key -> Transaction t m ()
deleteBS key = liftStateT . modify . Map.insert key $ Nothing

delete :: Monad m => Key -> Transaction t m ()
delete = deleteBS

lookup :: (Monad m, Binary a) => Key -> Transaction t m (Maybe a)
lookup = (liftM . fmap) decodeS . lookupBS

insert :: (Monad m, Binary a) => Key -> a -> Transaction t m ()
insert key = insertBS key . encodeS

writeGuid :: (Monad m, Binary a) => Guid -> a -> Transaction t m ()
writeGuid = insert

guidExists :: Monad m => Guid -> Transaction t m Bool
guidExists = liftM isJust . lookupBS

readGuidMb :: (Monad m, Binary a) => Transaction t m a -> Guid -> Transaction t m a
readGuidMb nothingCase guid =
  maybe nothingCase (return . decodeS) =<< lookupBS guid

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

fromIRef :: (Monad m, Binary a) => IRef a -> Property t m a
fromIRef iref = Property.Property (readIRef iref) (writeIRef iref)

fromIRefDef :: (Monad m, Binary a) => IRef a -> a -> Property t m a
fromIRefDef iref def = Property.Property (readIRefDef def iref) (writeIRef iref)

fromContainerRefI :: (Monad m, Binary a) =>
                     (Guid -> Transaction t m a) ->
                     ContainerRef a -> Container Guid t m a
fromContainerRefI guidReader containerRef guid = Property.Property (guidReader key) (writeGuid key)
  where
    key = ContainerRef.guidBase containerRef `Guid.xor` guid

fromContainerRef :: (Monad m, Binary a) => ContainerRef a -> Container Guid t m a
fromContainerRef = fromContainerRefI readGuid

fromContainerRefDef :: (Monad m, Binary a) => a -> ContainerRef a -> Container Guid t m a
fromContainerRefDef def = fromContainerRefI $ readGuidDef def

newKey :: Monad m => Transaction t m Key
newKey = liftInner . storeNewKey =<< liftReaderT ask

newIRef :: (Monad m, Binary a) => a -> Transaction t m (IRef a)
newIRef val = do
  newGuid <- newKey
  insert newGuid val
  return $ IRef.unsafeFromGuid newGuid

newContainerRef :: (Monad m, Binary a) => Transaction t m (ContainerRef a)
newContainerRef = ContainerRef.unsafeFromGuid `liftM` newKey

-- Dereference the *current* value of the IRef (Will not track new
-- values of IRef, by-value and not by-name)
followBy :: (Monad m, Binary a) =>
            (b -> IRef a) ->
            Property t m b ->
            Transaction t m (Property t m a)
followBy conv = liftM (fromIRef . conv) . Property.get

anchorRef :: (Monad m, Binary a) => String -> Property t m a
anchorRef = fromIRef . IRef.anchor

anchorRefDef :: (Monad m, Binary a) => String -> a -> Property t m a
anchorRefDef name def = flip fromIRefDef def . IRef.anchor $ name

anchorContainer :: (Monad m, Binary a) => String -> Container Guid t m a
anchorContainer = fromContainerRef . ContainerRef.anchor

anchorContainerDef :: (Monad m, Binary a) => String -> a -> Container Guid t m a
anchorContainerDef name def = fromContainerRefDef def . ContainerRef.anchor $ name

containerStr :: (Monad m, Binary a) =>
                Container Guid t m a ->
                Container String t m a
containerStr c = c . Guid.make . fromString

run :: Monad m => Store t m -> Transaction t m a -> m a
run store transaction = do
  (res, changes) <- (`runStateT` mempty) . (`runReaderT` store) . unTransaction $ transaction
  storeAtomicWrite store $ Map.toList changes
  return res
