{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Data.Cache.Types where

import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Typeable (Typeable)
import qualified Control.Lens.TH as LensTH
import qualified Data.ByteString as SBS

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
