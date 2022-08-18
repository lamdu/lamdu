module Tests.FuzzySearch (test) where

import qualified Control.Lens as Lens
import           Data.MRUMemo (memo)
import qualified Lamdu.Fuzzy as Fuzzy
import           Lamdu.Fuzzy (Fuzzy)

import           Test.Lamdu.Prelude

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

test :: TestTree
test =
    assertEqual "unexpected fuzzy search result" (match 0) (match 1)
    & testCase "fuzzy"
    where
        match i = matches ^? Lens.ix i . Lens._1
        matches = Fuzzy.memoableMake fuzzyMaker (["num", "not"] <&> (\x -> ([x], x))) "n"
