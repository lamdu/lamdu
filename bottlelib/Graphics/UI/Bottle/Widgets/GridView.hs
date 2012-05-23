{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.GridView(make, makeGeneric) where

import Control.Applicative (liftA2)
import Control.Arrow (second)
import Data.List (transpose, foldl')
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.SizeRange (fixedSize, SizeRange(..), Size)
import Graphics.UI.Bottle.Sized (Sized(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect
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
makeSizes [] = (SizeRange.fixedSize 0, const [])
makeSizes [[]] = (SizeRange.fixedSize 0, const [[]])
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
      (map (foldl' max 0 . map (f0 . SizeRange.srMinSize)) xs,
       map (fmap (foldl' max 0) . mapM (f1 . SizeRange.srMaxSize)) xs)

    rowHeightRanges = zip rowMinHeights rowMaxHeights
    columnWidthRanges = zip columnMinWidths columnMaxWidths
    mkSizes givenSize = map (Vector2.zip columnWidths . repeat) rowHeights
      where
        Vector2 extraWidth extraHeight = liftA2 (-) givenSize minSize
        columnWidths = disperse extraWidth columnWidthRanges
        rowHeights = disperse extraHeight rowHeightRanges

--- Placables:

makePlacements :: [[SizeRange]] -> (SizeRange, Size -> [[Rect]])
makePlacements = (fmap . second . fmap) placements makeSizes
  where
    placements sizes = (zipWith . zipWith) Rect positions sizes
      where
        positions =
          zipWith Vector2.zip
            (map (scanl (+) 0 . map Vector2.fst) sizes)
            (transpose . map (scanl (+) 0 . map Vector2.snd) . transpose $ sizes)

--- Displays:

-- Used by both make and Grid's make.
makeGeneric :: (Rect -> a -> b) -> [[Sized a]] -> Sized [[b]]
-- Special case to preserve shape to avoid handling it above in
-- "maximum", "transpose", etc
makeGeneric _ [] =
  Sized (fixedSize 0) (const [])
makeGeneric _ [[]] =
  Sized (fixedSize 0) (const [[]])
makeGeneric translate rows =
  Sized reqSize mkRes
  where
    (reqSize, mkPlacements) = makePlacements $ (map . map) requestedSize rows
    mkRes givenSize = (zipWith . zipWith) locate (mkPlacements givenSize) rows
    locate rect sized = translate rect . fromSize sized $ Rect.rectSize rect

make :: [[Sized Anim.Frame]] -> Sized Anim.Frame
make = fmap (mconcat . concat) . makeGeneric (Anim.translate . Rect.rectTopLeft)
