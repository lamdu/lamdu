{-# OPTIONS -Wall #-}
module Data.List.Utils
	( groupOn, sortOn, enumerate, enumerate2d, nth, index, removeAt, atPred
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

atPred
  :: (key -> Bool)
  -> (a -> a)
  -> [(key, a)]
  -> [(key, a)]
atPred p f xs =
  [ (key, if p key then f x else x)
  | (key, x) <- xs ]
