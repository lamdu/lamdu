module Data.List.Utils
    ( groupOn
    , sortOn
    , minimumOn
    , insertAt
    , removeAt
    , nonEmptyAll
    , match
    , isLengthAtLeast
    , withPrevNext
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Function (on)
import           Data.List (groupBy, sortBy, minimumBy)
import           Data.Ord (comparing)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

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

nonEmptyAll :: (a -> Bool) -> [a] -> Bool
nonEmptyAll _ [] = False
nonEmptyAll f xs = all f xs

match :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
match f (x:xs) (y:ys) =
    match f xs ys <&> (f x y :)
match _ [] [] = Just []
match _ _ _ = Nothing

withPrevNext :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
withPrevNext before after f list =
    zip3 (before : keys) (tail (keys ++ [after])) list
    where
        keys = map f list
