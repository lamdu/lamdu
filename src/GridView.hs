{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module GridView(make) where

import           Data.List                        (transpose)
import qualified Data.Label                       as Label
import           Data.Monoid                      (Monoid(..))
import           Data.Vector.Vector2              (Vector2(..))
import qualified Data.Vector.Vector2              as Vector2
import           Control.Compose                  (result)
import           Control.Applicative              (liftA2)
import           Control.Arrow                    (second)
import qualified Graphics.DrawingCombinators      as Draw
import           Graphics.DrawingCombinators      ((%%))
import           SizeRange                        (SizeRange(..), Size, Coordinate)
import qualified SizeRange                        as SizeRange

--- Size computations:

-- Give each min-max range some of the extra budget...
disperse :: (Ord a, Num a) => a -> [(a, Maybe a)] -> [a]
disperse _     [] = []
disperse extra ((low, high):xs) = r : disperse remaining xs
  where
    r = max low . maybe id min high $ low + extra
    remaining = low + extra - r

makeSizes :: [[SizeRange]] -> (SizeRange, Size -> [[Size]])
makeSizes rows = (requestedSize, mkSizes)
  where
    requestedSize = SizeRange minSize maxSize
    minSize = fmap sum $ Vector2 columnMinWidths rowMinHeights
    maxSize = fmap (fmap sum . sequence) $ Vector2 columnMaxWidths rowMaxHeights

    (rowMinHeights, rowMaxHeights) =
      computeSizeRanges Vector2.snd Vector2.snd rows
    (columnMinWidths, columnMaxWidths) =
      computeSizeRanges Vector2.fst Vector2.fst . transpose $ rows

    -- computeSizeRanges takes f twice (as f0 and f1) to work around
    -- lack of (proper) Rank2Types
    computeSizeRanges f0 f1 xs =
      (map (maximum . map (f0 . Label.get SizeRange.srMinSize)) xs,
       map (fmap maximum . mapM (\x -> f1 (Label.get SizeRange.srMaxSize x))) xs)

    rowHeightRanges = zip rowMinHeights rowMaxHeights
    columnWidthRanges = zip columnMinWidths columnMaxWidths
    mkSizes givenSize = map (Vector2.zip columnWidths . repeat) rowHeights
      where
        Vector2 extraWidth extraHeight = liftA2 (-) givenSize minSize
        columnWidths = disperse extraWidth columnWidthRanges
        rowHeights = disperse extraHeight rowHeightRanges

--- Placables:

type Placement = (Coordinate, Size)

makePlacements :: [[SizeRange]] -> (SizeRange, Size -> [[Placement]])
makePlacements = (result . second . result) placements makeSizes
  where
    placements sizes = zipWith zip positions sizes
      where
        positions =
          zipWith Vector2.zip
            (map (scanl (+) 0 . map Vector2.fst) sizes)
            (transpose . map (scanl (+) 0 . map Vector2.snd) . transpose $ sizes)

--- Displays:

make :: Monoid a => [[(SizeRange, Size -> Draw.Image a)]] -> (SizeRange, Size -> Draw.Image a)
make rows = (requestedSize, mkImage)
  where
    (requestedSize, mkPlacements) = makePlacements $ (map . map) fst rows
    mkImage givenSize =
      mconcat . concat $ (zipWith . zipWith) locate (mkPlacements givenSize) rows
    locate (pos, size) (_, f) = Draw.translate (Vector2.uncurry (,) pos) %% f size
