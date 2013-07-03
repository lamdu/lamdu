{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Data.Cache
  ( Cache, Key
  , new, peek, touch, lookup
  , FuncId
  , memo, memoS, unmemoS
  , lookupS
  -- For lower-level memoization
  , KeyBS, bsOfKey
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..), evalStateT, state, execState)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Binary.Utils (encodeS)
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
castUnmaybe suffix x =
  result
  where
    msg =
      concat
      [ "cast of ", show (typeOf x), " to ", show (typeOf result)
      , " attempted in (", suffix, ")"
      ]
    result = unsafeUnjust msg $ cast x

cacheValMap :: Typeable k => k -> Lens.Traversal' Cache ValMap
cacheValMap key = cEntries . Lens.ix (typeOf key)

lookupKey :: (Typeable k, Typeable v) => k -> Cache -> Maybe (ValEntry v)
lookupKey key cache =
  findKey =<< cache ^? cacheValMap key
  where
    findKey (ValMap m) =
      Map.lookup (castUnmaybe "lookupKey:key" key) m
      <&> (veValue %~ \(AnyVal val) -> castUnmaybe "lookupKey:val" val)

-- TODO: Convert to Lens.at so you can poke too
peek :: (Key k, Typeable v) => k -> Cache -> Maybe v
peek key cache = (^. veValue) <$> lookupKey key cache

insertNew :: Ord k => String -> k -> v -> Map k v -> Map k v
insertNew msg k v =
  Map.alter f k
  where
    f Nothing = Just v
    f (Just _) = error $ "insertNew found old key: " ++ show msg

addKey ::
  (Key k, Typeable v) => k -> ValEntry v ->
  Map TypeRep ValMap -> Map TypeRep ValMap
addKey key entry =
  Map.alter f $ typeOf key
  where
    anyValEntry = entry & veValue %~ AnyVal
    f Nothing = Just . ValMap $ Map.singleton key anyValEntry
    f (Just (ValMap valMap)) =
      Just . ValMap $
      insertNew "addKey" (castUnmaybe "addKey:key" key) anyValEntry valMap

lookupHelper :: Key k => r -> (Cache -> AnyVal -> r) -> k -> Cache -> r
lookupHelper onMiss onHit key cache =
  fromMaybe onMiss $ findKey =<< cache ^? cacheValMap key
  where
    findKey (ValMap m) =
      case Map.lookup ckey m of
      Nothing -> Nothing
      Just (ValEntry prevPriority val) ->
        Just $ onHit newCache val
        where
          newCache =
            cache
            & cEntries %~ Map.insert (typeOf key) (ValMap $ Map.insert ckey (ValEntry newPriority val) m)
            & cPriorities %~
              Map.insert newPriority (AnyKey key) .
              Map.delete prevPriority
            & cCounter +~ 1
          newPriority =
            prevPriority { pRecentUse = cache ^. cCounter }
      where
        ckey = castUnmaybe "lookupHelper:key" key

touch :: Key k => k -> Cache -> Cache
touch key cache = lookupHelper cache const key cache

lookup :: (Key k, Typeable v) => k -> Cache -> (Maybe v, Cache)
lookup key cache =
  lookupHelper (Nothing, cache) onHit key cache
  where
    onHit newCache (AnyVal val) = (Just (castUnmaybe "lookup:val" val), newCache)

lookupS :: MonadA m => (Key k, Typeable v) => k -> StateT Cache m (Maybe v)
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
          case Map.delete (castUnmaybe "lookupHelper:key" key) m of
          res
            | Map.null res -> Nothing
            | otherwise -> Just $ ValMap res

evict :: Cache -> Cache
evict cache
  | cache ^. cSize <= cMaxSize cache = cache
  | otherwise = evict $ evictLowestScore cache

-- Assumes key was not in the cache before.
insertHelper :: (Key k, Typeable v, Binary v) => k -> v -> Cache -> Cache
insertHelper key val cache =
  evict .
  (cSize +~ valLen) .
  (cEntries %~ addKey key entry) .
  (cPriorities %~ insertNew "insertHelper" priority (AnyKey key)) .
  (cCounter +~ 1) $
  cache
  where
    entry = ValEntry priority val
    valLen = SBS.length $ encodeS val
    priority = PriorityData (cache ^. cCounter) valLen

type FuncId = String

-- Actually requires only Pointed Functor.
memo ::
  (Key k, Binary v, Typeable v, MonadA m) =>
  FuncId -> (k -> m v) -> k -> Cache -> m (v, Cache)
memo funcId f rawKey cache =
  lookupHelper onMiss onHit key cache
  where
    key = (funcId, rawKey)
    onMiss = do
      val <- f rawKey
      return (val, insertHelper key val cache)
    onHit newCache (AnyVal val) = return (castUnmaybe (funcId ++ ":memo:val") val, newCache)

memoS ::
  (Key k, Binary v, Typeable v, MonadA m) =>
  FuncId -> (k -> m v) -> k -> StateT Cache m v
memoS funcId f key = StateT $ memo funcId f key

unmemoS :: MonadA m => StateT Cache m a -> m a
unmemoS = (`evalStateT` new 0)
