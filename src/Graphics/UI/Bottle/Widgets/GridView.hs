{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.GridView(
    make, makeGeneric, makeFromWidgets) where

import           Control.Newtype
import           Control.Applicative               (liftA2)
import           Control.Arrow                     (first, second)
import           Data.List                         (transpose)
import           Data.Monoid                       (Monoid(..))
import           Data.Vector.Vector2               (Vector2(..))
import           Graphics.DrawingCombinators       ((%%))
import           Graphics.DrawingCombinators.Utils (Image)
import           Graphics.UI.Bottle.SizeRange (SizeRange(..), Size, Coordinate)
import           Graphics.UI.Bottle.Sized     (Sized(..))
import           Graphics.UI.Bottle.Widget    (Widget(..))
import qualified Data.Record.Label                 as Label
import qualified Data.Vector.Vector2               as Vector2
import qualified Graphics.DrawingCombinators       as Draw
import qualified Graphics.UI.Bottle.SizeRange as SizeRange

--- Size computations:

-- Give each min-max range some of the extra budget...
disperse :: (Ord a, Num a) => a -> [(a, Maybe a)] -> [a]
disperse _     [] = []
disperse extra ((low, high):xs) = r : disperse remaining xs
  where
    r = max low . maybe id min high $ low + extra
    remaining = low + extra - r

makeSizes :: [[SizeRange]] -> (SizeRange, Size -> [[Size]])
makeSizes rows = (reqSize, mkSizes)
  where
    reqSize = SizeRange minSize maxSize
    minSize = fmap sum $ Vector2 columnMinWidths rowMinHeights
    maxSize = fmap (fmap sum . sequence) $ Vector2 columnMaxWidths rowMaxHeights

    (rowMinHeights, rowMaxHeights) =
      computeSizeRanges Vector2.snd Vector2.snd rows
    (columnMinWidths, columnMaxWidths) =
      computeSizeRanges Vector2.fst Vector2.fst . transpose $ rows

    -- computeSizeRanges takes f twice (as f0 and f1) to work around
    -- lack of (proper) Rank2Types
    computeSizeRanges f0 f1 xs =
      (map (maximum . map (f0 . Label.getL SizeRange.srMinSize)) xs,
       map (fmap maximum . mapM (f1 . Label.getL SizeRange.srMaxSize)) xs)

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
makePlacements = (fmap . second . fmap) placements makeSizes
  where
    placements sizes = zipWith zip positions sizes
      where
        positions =
          zipWith Vector2.zip
            (map (scanl (+) 0 . map Vector2.fst) sizes)
            (transpose . map (scanl (+) 0 . map Vector2.snd) . transpose $ sizes)

--- Displays:

-- Used by both make and Grid's make.
makeGeneric :: (a -> Image) -> [[Sized a]] -> Sized (Image, [[a]])
makeGeneric toImage rows =
  Sized reqSize mkRes
  where
    (reqSize, mkPlacements) = makePlacements $ (map . map) requestedSize rows
    mkRes givenSize =
      first (mconcat . concat) . unzip2d $
      (zipWith . zipWith) locate (mkPlacements givenSize) rows
    locate (pos, size) sized =
      (Draw.translate (Vector2.uncurry (,) pos) %% toImage res, res)
      where
        res = fromSize sized size

unzip2d :: [[(a, b)]] -> ([[a]], [[b]])
unzip2d = unzip . map unzip

make :: [[Sized Image]] -> Sized Image
make = fmap fst . makeGeneric id

-- ^ This will send events to the first widget in the list that would
-- take them. It is useful especially for lifting views to widgets and
-- composing them with widgets.
makeFromWidgets :: [[Widget k]] -> Widget k
makeFromWidgets widgets = Widget handleHasFocus
  where
    handleHasFocus hasFocus =
      (fmap . second) (mconcat . map snd . concat) $
      makeGeneric fst children
      where
        children = (map . map) (($ hasFocus) . op Widget) widgets
