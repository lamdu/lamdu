{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}

module Data.Cache
  ( Cache, Key
  , new, peek, touch, lookup, memo, memoS
  , lookupS
  -- For lower-level memoization
  , KeyBS, bsOfKey
  ) where

import Control.Lens ((%=), (-=))
import Control.Monad.Trans.State (StateT(..), state, execState)
import Data.Binary (Binary)
import Data.Binary.Utils (decodeS, encodeS)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Typeable (Typeable, typeOf)
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as SBS
import qualified Data.Map as Map

type Key a = (Binary a, Typeable a)

data PriorityData = PriorityData
  { pRecentUse :: Int64
  , pMemoryConsumption :: Int
  } deriving Eq

priorityScore :: PriorityData -> Int64
priorityScore (PriorityData use mem) = use - fromIntegral mem

instance Ord PriorityData where
  x <= y
    | x == y = True
    | px < py = True
    | px > py = False
    | otherwise = pRecentUse x < pRecentUse y
    where
      px = priorityScore x
      py = priorityScore y

type KeyBS = SBS.ByteString
type ValBS = SBS.ByteString
type ValEntry = (PriorityData, ValBS)

data Cache = Cache
  { _cEntries :: Map KeyBS ValEntry
  , _cPriorities :: Map PriorityData KeyBS
  , _cCounter :: Int64
  , _cSize :: Int
  , cMaxSize :: Int
  }

LensTH.makeLenses ''Cache

new :: Int -> Cache
new maxSize =
  Cache
  { _cEntries = Map.empty
  , _cPriorities = Map.empty
  , _cCounter = 0
  , _cSize = 0
  , cMaxSize = maxSize
  }

bsOfKey :: Key k => k -> KeyBS
bsOfKey key = SHA1.hash $ encodeS (show (typeOf key), key)

peek :: (Key k, Binary v) => k -> Cache -> Maybe v
peek key =
  fmap (decodeS . snd) . Map.lookup (bsOfKey key) . Lens.view cEntries

touchExisting :: Cache -> KeyBS -> ValEntry -> Cache
touchExisting cache bsKey (prevPriority, bsVal) =
  Lens.over cEntries (Map.insert bsKey (newPriority, bsVal)) .
  Lens.over cPriorities
    (Map.insert newPriority bsKey . Map.delete prevPriority) .
  Lens.over cCounter (+1) $
  cache
  where
    newPriority =
      prevPriority { pRecentUse = Lens.view cCounter cache }

lookupHelper :: Key k => (KeyBS -> r) -> (KeyBS -> ValEntry -> r) -> k -> Cache -> r
lookupHelper onMiss onHit key cache =
  maybe (onMiss bsKey) (onHit bsKey) .
  Map.lookup bsKey $ Lens.view cEntries cache
  where
    bsKey = bsOfKey key

touch :: Key k => k -> Cache -> Cache
touch key cache =
  lookupHelper (const cache) (touchExisting cache) key cache

lookup :: (Key k, Binary v) => k -> Cache -> (Maybe v, Cache)
lookup key cache =
  lookupHelper (const (Nothing, cache)) onHit key cache
  where
    onHit bsKey entry@(_, bsVal) =
      ( Just $ decodeS bsVal
      , touchExisting cache bsKey entry
      )

lookupS :: Monad m => (Key k, Binary v) => k -> StateT Cache m (Maybe v)
lookupS = state . lookup

evictLowestScore :: Cache -> Cache
evictLowestScore =
  execState $
  clearEntry =<<
  Lens.zoom cPriorities
  (state Map.deleteFindMin)
  where
    clearEntry (priorityData, bsKey) = do
      cSize -= pMemoryConsumption priorityData
      cEntries %= Map.delete bsKey

evict :: Cache -> Cache
evict cache
  | Lens.view cSize cache <= cMaxSize cache = cache
  | otherwise = evict $ evictLowestScore cache

-- Assumes key was not in the cache before.
insertHelper :: Binary v => KeyBS -> v -> Cache -> Cache
insertHelper bsKey val cache =
  evict .
  Lens.over cSize (+valLen) .
  Lens.over cEntries (Map.insert bsKey entry) .
  Lens.over cPriorities (Map.insert priority bsKey) .
  Lens.over cCounter (+1) $
  cache
  where
    entry = (priority, bsVal)
    bsVal = encodeS val
    valLen = SBS.length bsVal
    priority = PriorityData (Lens.view cCounter cache) valLen

-- Actually requires only Pointed Functor.
memo ::
  (Key k, Binary v, Monad m) =>
  (k -> m v) -> k -> Cache -> m (v, Cache)
memo f key cache =
  lookupHelper onMiss onHit key cache
  where
    onMiss bsKey = do
      val <- f key
      return (val, insertHelper bsKey val cache)
    onHit bsKey entry@(_, bsVal) =
      return (decodeS bsVal, touchExisting cache bsKey entry)

memoS :: (Key k, Binary v, Monad m) => (k -> m v) -> k -> StateT Cache m v
memoS f key = StateT $ memo f key
