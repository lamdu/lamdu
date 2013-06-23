{-# LANGUAGE ConstraintKinds, TemplateHaskell, ExistentialQuantification #-}
module Data.Cache.Types where

import Data.Int (Int64)
import Data.Map (Map)
import Data.Typeable (Typeable, TypeRep)
import qualified Control.Lens as Lens
import qualified Data.ByteString as SBS

type Key k = (Typeable k, Ord k)

data PriorityData = PriorityData
  { pRecentUse :: Int64
  , pMemoryConsumption :: Int
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
type ValEntry = (PriorityData, ValBS)

data ValMap = forall k. Key k => ValMap (Map k ValEntry)
data AnyKey = forall k. Key k => AnyKey k

data Cache = Cache
  { _cEntries :: Map TypeRep ValMap
  , _cPriorities :: Map PriorityData AnyKey
  , _cCounter :: Int64
  , _cSize :: Int
  , cMaxSize :: Int
  }

Lens.makeLenses ''Cache
