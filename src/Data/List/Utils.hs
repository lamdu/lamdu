{-# OPTIONS -Wall #-}
module Data.List.Utils(enumerate, enumerate2d) where

enumerate :: (Enum a, Num a) => [b] -> [(a, b)]
enumerate = zip [0..]

enumerate2d :: (Enum a, Num a) => [[b]] -> [[((a, a), b)]]
enumerate2d = map f . enumerate . map enumerate
  where
    f (rowIndex, row) = map (g rowIndex) row
    g rowIndex (colIndex, x) = ((rowIndex, colIndex), x)
