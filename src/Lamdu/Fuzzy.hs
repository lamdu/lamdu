-- | A wrapper for the fuzzyset library that makes it a Fuzzy map
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Fuzzy
    ( FuzzySet, FuzzyMap, make, matches
    , fuzzyMaker
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

fuzzyMaker :: [Text] -> FuzzySet
fuzzyMaker = Fuzzy.fromList

make :: ([Text] -> FuzzySet) -> (a -> [Text]) -> [a] -> FuzzyMap a
make maker f opts =
    FuzzyMap
    { fuzzySet = Map.keys vals & maker
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
