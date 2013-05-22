module Data.List.Utils
  ( groupOn
  , sortOn
  , enumerate
  , enumerate2d
  , nth
  , index
  , insertAt
  , removeAt
  , pairList
  , theOne
  , nonEmptyAll
  , match
  , isLengthAtLeast
  ) where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

enumerate2d :: [[a]] -> [[((Int, Int), a)]]
enumerate2d = map f . enumerate . map enumerate
  where
    f (rowIndex, row) = map (g rowIndex) row
    g rowIndex (colIndex, x) = ((rowIndex, colIndex), x)

nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = error "Apply out of bounds"
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n-1) f xs

index :: Int -> [a] -> Maybe a
index n = listToMaybe . drop n

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = take n xs ++ x : drop n xs

pairList :: (a, a) -> [a]
pairList (x, y) = [x, y]

theOne :: [a] -> Maybe a
theOne [x] = Just x
theOne _ = Nothing

isLengthAtLeast :: Int -> [a] -> Bool
isLengthAtLeast l _ | l <= 0 = True
isLengthAtLeast _ [] = False
isLengthAtLeast l (_:xs) = isLengthAtLeast (l-1) xs

nonEmptyAll :: (a -> Bool) -> [a] -> Bool
nonEmptyAll _ [] = False
nonEmptyAll f xs = all f xs

match :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
match f (x:xs) (y:ys) =
  fmap (f x y :) $ match f xs ys
match _ [] [] = Just []
match _ _ _ = Nothing
