module Data.List.Extended
    ( module Data.List
    , groupOn
    , minimumOn
    , insertAt
    , removeAt
    , isLengthAtLeast
    , withPrevNext
    , rightPad
    ) where

import qualified Control.Lens as Lens
import           Data.Function (on)
import           Data.List
import           Data.Ord (comparing)

import           Prelude

rightPad :: Int -> a -> [a] -> [a]
rightPad l x xs
    | len >= l = xs
    | otherwise = xs ++ replicate (l - len) x
    where
        len = length xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = take n xs ++ x : drop n xs

isLengthAtLeast :: Int -> [a] -> Bool
isLengthAtLeast n
    | n <= 0 = const True
    | otherwise = Lens.has (Lens.ix (n-1))

withPrevNext :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
withPrevNext before after f list =
    zip3 (before : keys) (tail (keys ++ [after])) list
    where
        keys = map f list
