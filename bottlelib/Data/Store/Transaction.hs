{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TemplateHaskell #-}

module Data.Store.Transaction
  ( Transaction, run
  , Store(..), onStoreM
  , Changes, fork, merge
  , forkScratch
  , lookupBS, lookup
  , insertBS, insert
  , delete, deleteIRef
  , readIRef, readIRefDef, writeIRef
  , readGuid, readGuidDef, writeGuid
  , isEmpty
  , guidExists, irefExists
  , newIRef, newKey
  , followBy
  , anchorRef, anchorRefDef
  , assocDataRef, assocDataRefDef
  , Property
  , fromIRef, fromIRefDef
  , MkProperty(..), mkProperty, mkPropertyFromIRef
  , getP, setP, modP
  )
where

import Control.Applicative (Applicative, (<|>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
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
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property

type ChangesMap = Map Key (Maybe Value)

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

data Askable m = Askable
  { _aStore :: Store m
  , _aBase :: ChangesMap
  }
Lens.makeLenses ''Askable

-- Define transformer stack:
newtype Transaction m a =
  Transaction (ReaderT (Askable m) (StateT ChangesMap m) a)
  deriving (Monad, Applicative, Functor)
liftAskable :: ReaderT (Askable m) (StateT ChangesMap m) a -> Transaction m a
liftAskable = Transaction
liftChangesMap :: MonadA m => StateT ChangesMap m a -> Transaction m a
liftChangesMap = liftAskable . lift
liftInner :: MonadA m => m a -> Transaction m a
liftInner = Transaction . lift . lift

getStore :: Monad m => Transaction m (Store m)
getStore = liftAskable $ Lens.view aStore

getBase :: Monad m => Transaction m ChangesMap
getBase = liftAskable $ Lens.view aBase

run :: MonadA m => Store m -> Transaction m a -> m a
run store (Transaction transaction) = do
  (res, changes) <-
    transaction
    & (`runReaderT` Askable store mempty)
    & (`runStateT` mempty)
  storeAtomicWrite store $ Map.toList changes
  return res

-- | Run the given transaction in a new "scratch" space forked from
-- the current transaction.
forkScratch :: MonadA m => Transaction m a -> Transaction m a
forkScratch = fmap fst . fork

newtype Changes = Changes ChangesMap

-- | Fork the given transaction into its own space.  Unless the
-- transaction is later "merged" into the main transaction, its
-- changes will not be committed anywhere.
fork :: MonadA m => Transaction m a -> Transaction m (a, Changes)
fork (Transaction discardableTrans) = do
  changes <- liftChangesMap State.get
  -- Map.union is left-biased, so changes correctly override base:
  askable <- liftAskable Reader.ask <&> aBase %~ Map.union changes
  discardableTrans
    & (`runReaderT` askable)
    & (`runStateT` mempty)
    & liftInner
    <&> Lens._2 %~ Changes

merge :: MonadA m => Changes -> Transaction m ()
merge (Changes changes) =
  liftChangesMap . State.modify $ Map.union changes

isEmpty :: MonadA m => Transaction m Bool
isEmpty = liftChangesMap (State.gets Map.null)

lookupBS :: MonadA m => Guid -> Transaction m (Maybe Value)
lookupBS guid = do
  base <- getBase
  changes <- liftChangesMap State.get
  case Map.lookup guid changes <|> Map.lookup guid base of
    Nothing -> do
      store <- getStore
      liftInner $ storeLookup store guid
    Just res -> return res

insertBS :: MonadA m => Guid -> ByteString -> Transaction m ()
insertBS key = liftChangesMap . State.modify . Map.insert key . Just

delete :: MonadA m => Guid -> Transaction m ()
delete key = liftChangesMap . State.modify . Map.insert key $ Nothing

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

newKey :: MonadA m => Transaction m Key
newKey = liftInner . storeNewKey =<< getStore

newIRef :: (MonadA m, Binary a) => a -> Transaction m (IRef (Tag m) a)
newIRef val = do
  newGuid <- newKey
  insert newGuid val
  return $ IRef.unsafeFromGuid newGuid

---------- Properties:

type Property m = Property.Property (Transaction m)

fromIRef :: (MonadA m, Binary a) => IRef (Tag m) a -> Transaction m (Property m a)
fromIRef iref = fmap (flip Property.Property (writeIRef iref)) $ readIRef iref

fromIRefDef :: (MonadA m, Binary a) => IRef (Tag m) a -> a -> Transaction m (Property m a)
fromIRefDef iref def = fmap (flip Property.Property (writeIRef iref)) $ readIRefDef def iref

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

newtype MkProperty m a = MkProperty { _mkProperty :: Transaction m (Property m a) }
Lens.makeLenses ''MkProperty

mkPropertyFromIRef :: (MonadA m, Binary a) => IRef (Tag m) a -> MkProperty m a
mkPropertyFromIRef = MkProperty . fromIRef

getP :: MonadA m => MkProperty m a -> Transaction m a
getP = fmap Property.value . (^. mkProperty)

setP :: MonadA m => MkProperty m a -> a -> Transaction m ()
setP (MkProperty mkProp) val = do
  prop <- mkProp
  Property.set prop val

modP :: MonadA m => MkProperty m a -> (a -> a) -> Transaction m ()
modP (MkProperty mkProp) f = do
  prop <- mkProp
  Property.pureModify prop f

assocDataRef :: (Binary a, MonadA m) => ByteString -> Guid -> MkProperty m (Maybe a)
assocDataRef str guid = MkProperty $ do
  val <- lookup assocGuid
  return $ Property.Property val set
  where
    assocGuid = Guid.combine guid $ Guid.make str
    set Nothing = delete assocGuid
    set (Just x) = writeGuid assocGuid x

assocDataRefDef :: (Eq a, Binary a, MonadA m) => a -> ByteString -> Guid -> MkProperty m a
assocDataRefDef def str guid =
  assocDataRef str guid
  & mkProperty . Lens.mapped %~ Property.pureCompose (fromMaybe def) f
  where
    f x
      | x == def = Nothing
      | otherwise = Just x
