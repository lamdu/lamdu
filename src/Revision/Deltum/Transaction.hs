{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

module Revision.Deltum.Transaction
    ( Transaction, run
    , Store(..), onStoreM
    , Changes, fork, merge
    , lookupBS, lookup
    , insertBS, insert
    , delete, deleteIRef
    , readIRef, writeIRef
    , isEmpty
    , irefExists
    , newIRef, newKey
    , assocDataRef, assocDataRefDef
    , fromIRef
    , mkPropertyFromIRef
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State
import           Data.Binary.Extended (encodeS, decodeS)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Property (Property, MkProperty')
import qualified Data.Property as Property
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Rev.Change (Key, Value)

import           Lamdu.Prelude hiding (lookup)

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
    , -- | The base is the set of changes we inherited from a parent
      -- transaction (in a fork) that we do NOT apply when merging the
      -- forked transaction.
      -- This in contrast to the Stateful ChangesMap that starts empty
      -- in the fork and is applied when the fork is merged.
      _aBase :: ChangesMap
    }
Lens.makeLenses ''Askable

-- Define transformer stack:
newtype Transaction m a =
    Transaction (ReaderT (Askable m) (StateT ChangesMap m) a)
    deriving stock Generic
    deriving newtype (Functor, Applicative, Monad)
    deriving (Semigroup, Monoid) via Monoid.Ap (Transaction m) a

type T = Transaction

liftAskable :: ReaderT (Askable m) (StateT ChangesMap m) a -> T m a
liftAskable = Transaction
liftChangesMap :: Monad m => StateT ChangesMap m a -> T m a
liftChangesMap = liftAskable . lift
liftInner :: Monad m => m a -> T m a
liftInner = Transaction . lift . lift

getStore :: Monad m => T m (Store m)
getStore = Lens.view aStore & liftAskable

getBase :: Monad m => T m ChangesMap
getBase = Lens.view aBase & liftAskable

run :: Monad m => Store m -> T m a -> m a
run store (Transaction transaction) = do
    (res, changes) <-
        transaction
        & (`runReaderT` Askable store mempty)
        & (`runStateT` mempty)
    storeAtomicWrite store $ Map.toList changes
    pure res

newtype Changes = Changes ChangesMap

-- | Fork the given transaction into its own space.  Unless the
-- transaction is later "merged" into the main transaction, its
-- changes will not be committed anywhere.
fork :: Monad m => T m a -> T m (a, Changes)
fork (Transaction discardableTrans) = do
    changes <- liftChangesMap State.get
    -- Map.union is left-biased, so changes correctly override base:
    askable <- liftAskable Reader.ask <&> aBase %~ Map.union changes
    discardableTrans
        & (`runReaderT` askable)
        & (`runStateT` mempty)
        & liftInner
        <&> _2 %~ Changes

merge :: Monad m => Changes -> T m ()
merge (Changes changes) =
    liftChangesMap . State.modify $ Map.union changes

isEmpty :: Monad m => T m Bool
isEmpty = liftChangesMap (State.gets Map.null)

lookupBS :: Monad m => UUID -> T m (Maybe Value)
lookupBS uuid = do
    base <- getBase
    changes <- liftChangesMap State.get
    case Map.lookup uuid changes <|> Map.lookup uuid base of
        Nothing -> do
            store <- getStore
            liftInner $ storeLookup store uuid
        Just res -> pure res

insertBS :: Monad m => UUID -> ByteString -> T m ()
insertBS key = liftChangesMap . State.modify . Map.insert key . Just

delete :: Monad m => UUID -> T m ()
delete key = liftChangesMap . State.modify . Map.insert key $ Nothing

lookup :: (Monad m, Binary a) => UUID -> T m (Maybe a)
lookup = (fmap . fmap) decodeS . lookupBS

insert :: (Monad m, Binary a) => UUID -> a -> T m ()
insert key = insertBS key . encodeS

writeUUID :: (Monad m, Binary a) => UUID -> a -> T m ()
writeUUID = insert

uuidExists :: Monad m => UUID -> T m Bool
uuidExists = fmap (Lens.has Lens._Just) . lookupBS

readUUIDMb :: (Monad m, Binary a) => T m a -> UUID -> T m a
readUUIDMb nothingCase uuid =
    maybe nothingCase pure =<< lookup uuid

readUUID :: (Monad m, Binary a) => UUID -> T m a
readUUID uuid = readUUIDMb failure uuid
    where
        failure = error $ "Inexistent uuid: " ++ show uuid ++ " referenced"

deleteIRef :: Monad m => IRef m a -> T m ()
deleteIRef = delete . IRef.uuid

readIRef :: (Monad m, Binary a) => IRef m a -> T m a
readIRef = readUUID . IRef.uuid

irefExists :: Monad m => IRef m a -> T m Bool
irefExists = uuidExists . IRef.uuid

writeIRef :: (Monad m, Binary a) => IRef m a -> a -> T m ()
writeIRef = writeUUID . IRef.uuid

newKey :: Monad m => T m Key
newKey = liftInner . storeNewKey =<< getStore

newIRef :: (Monad m, Binary a) => a -> T m (IRef m a)
newIRef val = do
    newUUID <- newKey
    insert newUUID val
    pure $ IRef.unsafeFromUUID newUUID

---------- Properties:

fromIRef :: (Monad m, Binary a) => IRef m a -> T m (Property (T m) a)
fromIRef iref = flip Property.Property (writeIRef iref) <$> readIRef iref

mkPropertyFromIRef :: (Monad m, Binary a) => IRef m a -> MkProperty' (T m) a
mkPropertyFromIRef = Property.MkProperty . fromIRef

assocDataRef :: (Binary a, Monad m) => ByteString -> UUID -> MkProperty' (T m) (Maybe a)
assocDataRef str uuid =
    lookup assocUUID <&> (`Property.Property` set) & Property.MkProperty
    where
        assocUUID = UUIDUtils.augment str uuid
        set Nothing = delete assocUUID
        set (Just x) = writeUUID assocUUID x

assocDataRefDef ::
    (Eq a, Binary a, Monad m) => a -> ByteString -> UUID -> MkProperty' (T m) a
assocDataRefDef def str uuid =
    assocDataRef str uuid
    & Property.prop %~ Property.pureCompose (fromMaybe def) f
    where
        f x
            | x == def = Nothing
            | otherwise = Just x
