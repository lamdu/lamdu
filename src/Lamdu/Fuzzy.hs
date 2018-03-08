-- | A wrapper for the fuzzyset library that makes it a Fuzzy map
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Fuzzy
    ( FuzzyMap, make, matches
    ) where

import           Data.FuzzySet (FuzzySet)
import qualified Data.FuzzySet as Fuzzy
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Lamdu.Prelude

data FuzzyMap a = FuzzyMap
    { fuzzySet :: FuzzySet
    , values :: Map Text a
    }

make :: (a -> Text) -> [a] -> FuzzyMap a
make f opts =
    FuzzyMap
    { fuzzySet = Map.keys vals & Fuzzy.fromList
    , values = vals
    }
    where
        vals = opts <&> join (,) <&> _1 %~ f & Map.fromList

matches :: Text -> FuzzyMap a -> [a]
matches text fuzzyMap =
    Fuzzy.get (fuzzySet fuzzyMap) text <&> snd
    <&> (`Map.lookup` values fuzzyMap)
    <&> fromMaybe (error "Element in fuzzy set not inside fuzzy map")
