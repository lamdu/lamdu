{-# LANGUAGE ConstraintKinds #-}
module Data.Cache
  ( Cache, Key
  , new, peek, touch, lookup, memo, memoS, unmemoS
  , lookupS
  -- For lower-level memoization
  , KeyBS, bsOfKey
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..), evalStateT, state, execState)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Binary.Utils (decodeS, encodeS)
import Data.Cache.Types
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Typeable (Typeable, TypeRep, typeOf, cast)
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

bsOfKey :: (Binary k, Typeable k) => k -> KeyBS
bsOfKey key = SHA1.hash $ encodeS (show (typeOf key), key)

castUnmaybe :: (Typeable a, Typeable b) => String -> a -> b
castUnmaybe suffix x = unsafeUnjust ("cast shouldn't fail, we use typeOf maps (" ++ suffix ++ ")") $ cast x

cacheValMap :: Typeable key => key -> Cache -> Maybe ValMap
cacheValMap key cache = cache ^? cEntries . Lens.ix (typeOf key)

lookupKey :: Typeable k => k -> Cache -> Maybe ValEntry
lookupKey key cache =
  findKey =<< cacheValMap key cache
  where
    findKey (ValMap m) = Map.lookup (castUnmaybe "1" key) m

-- TODO: Convert to Lens.at so you can poke too
peek :: (Key k, Binary v) => k -> Cache -> Maybe v
peek key cache = decodeS . snd <$> lookupKey key cache

addKey ::
  Key k => k -> ValEntry ->
  Map TypeRep ValMap -> Map TypeRep ValMap
addKey key entry =
  Map.alter f $ typeOf key
  where
    f Nothing = Just . ValMap $ Map.singleton key entry
    f (Just (ValMap valMap)) =
      Just . ValMap $ Map.insert (castUnmaybe "2" key) entry valMap

touchExisting :: Key k => k -> Cache -> ValEntry -> Cache
touchExisting key cache (prevPriority, bsVal) =
  cache
  & cEntries %~ addKey key valEntry
  & cPriorities %~
    Map.insert newPriority (AnyKey key) .
    Map.delete prevPriority
  & cCounter +~ 1
  where
    valEntry = (newPriority, bsVal)
    newPriority =
      prevPriority { pRecentUse = cache ^. cCounter }

lookupHelper :: Key k => r -> (Cache -> ValBS -> r) -> k -> Cache -> r
lookupHelper onMiss onHit key cache =
  fromMaybe onMiss $ findKey =<< cacheValMap key cache
  where
    findKey (ValMap m) =
      case Map.lookup (castUnmaybe "1" key) m of
      Nothing -> Nothing
      Just valEntry@(_, val) -> Just $ onHit (touchExisting key cache valEntry) val

touch :: Key k => k -> Cache -> Cache
touch key cache =
  lookupHelper cache (\newCache _val -> newCache) key cache

lookup :: (Key k, Binary v) => k -> Cache -> (Maybe v, Cache)
lookup key cache =
  lookupHelper (Nothing, cache) onHit key cache
  where
    onHit newCache bsVal = (Just (decodeS bsVal), newCache)

lookupS :: MonadA m => (Key k, Binary v) => k -> StateT Cache m (Maybe v)
lookupS = state . lookup

evictLowestScore :: Cache -> Cache
evictLowestScore =
  execState $
  clearEntry =<<
  Lens.zoom cPriorities (state Map.deleteFindMin)
  where
    clearEntry (priorityData, AnyKey key) = do
      cSize -= pMemoryConsumption priorityData
      cEntries %= Map.alter deleteKey (typeOf key)
      where
        deleteKey Nothing = error "Key pointed by cPriorities not found in Cache?!"
        deleteKey (Just (ValMap m)) =
          case Map.delete (castUnmaybe "3" key) m of
          res
            | Map.null res -> Nothing
            | otherwise -> Just $ ValMap res

evict :: Cache -> Cache
evict cache
  | cache ^. cSize <= cMaxSize cache = cache
  | otherwise = evict $ evictLowestScore cache

-- Assumes key was not in the cache before.
insertHelper :: (Key k, Binary v) => k -> v -> Cache -> Cache
insertHelper key val cache =
  evict .
  (cSize +~ valLen) .
  (cEntries %~ addKey key entry) .
  (cPriorities %~ Map.insert priority (AnyKey key)) .
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
    onMiss = do
      val <- f key
      return (val, insertHelper key val cache)
    onHit newCache bsVal = return (decodeS bsVal, newCache)

memoS :: (Key k, Binary v, MonadA m) => (k -> m v) -> k -> StateT Cache m v
memoS f key = StateT $ memo f key

unmemoS :: MonadA m => StateT Cache m a -> m a
unmemoS = (`evalStateT` new 0)
