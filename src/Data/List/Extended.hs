module Data.List.Extended
    ( insertAt
    , removeAt
    , isLengthAtLeast
    , withPrevNext
    , rightPad
    , module Data.List.Extended.Momentu
    ) where

import qualified Control.Lens as Lens
import           Data.List.Extended.Momentu

import           Lamdu.Prelude

rightPad :: Int -> a -> [a] -> [a]
rightPad l x xs
    | len >= l = xs
    | otherwise = xs ++ replicate (l - len) x
    where
        len = length xs

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
