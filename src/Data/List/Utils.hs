module Data.List.Utils
    ( groupOn
    , minimumOn
    , removeAt
    , nonEmptyAll
    , isLengthAtLeast
    , withPrevNext
    , rightPad
    ) where

import qualified Control.Lens as Lens
import           Data.Function (on)
import           Data.List (groupBy, minimumBy, maximumBy)
import           Data.Ord (comparing)

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

isLengthAtLeast :: Int -> [a] -> Bool
isLengthAtLeast n
    | n <= 0 = const True
    | otherwise = Lens.has (Lens.ix (n-1))

nonEmptyAll :: (a -> Bool) -> [a] -> Bool
nonEmptyAll _ [] = False
nonEmptyAll f xs = all f xs

withPrevNext :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
withPrevNext before after f list =
    zip3 (before : keys) (tail (keys ++ [after])) list
    where
        keys = map f list
