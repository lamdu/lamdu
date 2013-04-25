{-# LANGUAGE ConstraintKinds #-}
module Data.Cache
  ( Cache, Key
  , new, peek, touch, lookup, memo, memoS, unmemoS
  , lookupS
  -- For lower-level memoization
  , KeyBS, bsOfKey
  ) where

import Control.Lens ((&), (%=), (-=), (%~), (+~), (^.))
import Control.Monad.Trans.State (StateT(..), evalStateT, state, execState)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Binary.Utils (decodeS, encodeS)
import Data.Cache.Types
import Data.Typeable (typeOf)
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as SBS
import qualified Data.Map as Map

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

-- TODO: Convert to Lens.at so you can poke too
peek :: (Key k, Binary v) => k -> Cache -> Maybe v
peek key =
  fmap (decodeS . snd) . Map.lookup (bsOfKey key) . Lens.view cEntries

touchExisting :: Cache -> KeyBS -> ValEntry -> Cache
touchExisting cache bsKey (prevPriority, bsVal) =
  cache
  & cEntries %~ Map.insert bsKey (newPriority, bsVal)
  & cPriorities %~
    Map.insert newPriority bsKey .
    Map.delete prevPriority
  & cCounter +~ 1
  where
    newPriority =
      prevPriority { pRecentUse = cache ^. cCounter }

lookupHelper :: Key k => (KeyBS -> r) -> (KeyBS -> ValEntry -> r) -> k -> Cache -> r
lookupHelper onMiss onHit key cache =
  maybe (onMiss bsKey) (onHit bsKey) .
  Map.lookup bsKey $ cache ^. cEntries
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

lookupS :: MonadA m => (Key k, Binary v) => k -> StateT Cache m (Maybe v)
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
  | cache ^. cSize <= cMaxSize cache = cache
  | otherwise = evict $ evictLowestScore cache

-- Assumes key was not in the cache before.
insertHelper :: Binary v => KeyBS -> v -> Cache -> Cache
insertHelper bsKey val cache =
  evict .
  (cSize +~ valLen) .
  (cEntries %~ Map.insert bsKey entry) .
  (cPriorities %~ Map.insert priority bsKey) .
  (cCounter +~ 1) $
  cache
  where
    entry = (priority, bsVal)
    bsVal = encodeS val
    valLen = SBS.length bsVal
    priority = PriorityData (cache ^. cCounter) valLen

-- Actually requires only Pointed Functor.
memo ::
  (Key k, Binary v, MonadA m) =>
  (k -> m v) -> k -> Cache -> m (v, Cache)
memo f key cache =
  lookupHelper onMiss onHit key cache
  where
    onMiss bsKey = do
      val <- f key
      return (val, insertHelper bsKey val cache)
    onHit bsKey entry@(_, bsVal) =
      return (decodeS bsVal, touchExisting cache bsKey entry)

memoS :: (Key k, Binary v, MonadA m) => (k -> m v) -> k -> StateT Cache m v
memoS f key = StateT $ memo f key

unmemoS :: MonadA m => StateT Cache m a -> m a
unmemoS = (`evalStateT` new 0)
