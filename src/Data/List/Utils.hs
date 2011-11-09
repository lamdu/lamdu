{-# OPTIONS -Wall #-}
module Data.List.Utils(enumerate, enumerate2d, nth) where

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

enumerate2d :: (Enum a, Num a) => [[b]] -> [[((a, a), b)]]
enumerate2d = map f . enumerate . map enumerate
  where
    f (rowIndex, row) = map (g rowIndex) row
    g rowIndex (colIndex, x) = ((rowIndex, colIndex), x)

nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = error "Apply out of bounds"
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n-1) f xs
