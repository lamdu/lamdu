-- | A wrapper for the fuzzyset library that makes it a Fuzzy map
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Fuzzy
    ( FuzzyMap, make, matches
    ) where

import           Data.Function (on)
import           Data.FuzzySet (FuzzySet)
import qualified Data.FuzzySet as Fuzzy
import           Data.List (nubBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Lamdu.Prelude

type UniqueId = Int

data FuzzyMap a = FuzzyMap
    { fuzzySet :: FuzzySet
    , values :: Map Text (UniqueId, a)
    }

make :: (a -> [Text]) -> [a] -> FuzzyMap a
make f opts =
    FuzzyMap
    { fuzzySet = Map.keys vals & Fuzzy.fromList
    , values = vals
    }
    where
        textKey (idx, val) = f val <&> ((,) ?? (idx, val))
        vals = zip [0..] opts >>= textKey & Map.fromList

matches :: Text -> FuzzyMap a -> [a]
matches text fuzzyMap =
    Fuzzy.get (fuzzySet fuzzyMap) text
    <&> snd
    <&> (`Map.lookup` values fuzzyMap)
    <&> fromMaybe (error "Element in fuzzy set not inside fuzzy map")
    & nubBy ((==) `on` fst)
    <&> snd
