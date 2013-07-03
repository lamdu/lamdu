{-# LANGUAGE ConstraintKinds, TemplateHaskell, ExistentialQuantification #-}
module Data.Cache.Types where

import Data.Int (Int64)
import Data.Map (Map)
import Data.Typeable (Typeable, TypeRep)
import qualified Control.Lens as Lens
import qualified Data.ByteString as SBS

type Key k = (Typeable k, Ord k)

data PriorityData = PriorityData
  { pRecentUse :: {-# UNPACK #-}!Int64
  , pMemoryConsumption :: {-# UNPACK #-}!Int
  } deriving Eq

priorityScore :: PriorityData -> Int64
priorityScore (PriorityData use mem) = use*use - fromIntegral mem

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

data ValEntry v = ValEntry
  { _vePriorityData :: {-# UNPACK #-}!PriorityData
  , _veValue :: v
  }
Lens.makeLenses ''ValEntry

data AnyVal = forall v. Typeable v => AnyVal v
data ValMap = forall k. Key k => ValMap (Map k (ValEntry AnyVal))
data AnyKey = forall k. Key k => AnyKey k

data Cache = Cache
  { _cEntries :: Map TypeRep ValMap
  , _cPriorities :: Map PriorityData AnyKey
  , _cCounter :: Int64
  , _cSize :: Int
  , cMaxSize :: Int
  }

Lens.makeLenses ''Cache
