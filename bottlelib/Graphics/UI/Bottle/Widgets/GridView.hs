{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.GridView
  ( make, makeAlign, makeCentered
  , makeGeneric
  , Alignment
  ) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second, (***))
import Data.List (transpose)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect

data ComputedSizes = ComputedSizes
  { columnSizes :: [Anim.R]
  , rowSizes :: [Anim.R]
  }

totalSize :: ComputedSizes -> Anim.Size
totalSize = liftA2 Vector2 (sum . columnSizes) (sum . rowSizes)

computeSizes :: [[Anim.Size]] -> ComputedSizes
computeSizes rows = ComputedSizes
  { columnSizes = map (maximum . map Vector2.fst) columns
  , rowSizes = map (maximum . map Vector2.snd) rows
  }
  where
    columns = transpose rows

--- Placables:

type Alignment = Vector2 Anim.R -- ^ 0..1

makePlacements :: [[(Anim.Size, Alignment)]] -> (Anim.Size, [[Rect]])
makePlacements szAlignments =
  (totalSize cSizes, (zipWith . zipWith) mkRect allPlacements szAlignments)
  where
    mkRect (pos, cellSize) (size, alignment) =
      Rect (pos + alignment * (cellSize - size)) size
    cSizes = computeSizes $ (map . map) fst szAlignments
    allPlacements = map placeColumnsInRow rowPlacements
    placeColumnsInRow (y, height) =
      map ((`Vector2` y) *** (`Vector2` height)) columnPlacements
    pairs sizes = zip (scanl (+) 0 sizes) sizes
    rowPlacements = pairs $ rowSizes cSizes
    columnPlacements = pairs $ columnSizes cSizes

--- Displays:

-- Used by both make and Grid's make.
makeGeneric :: (Rect -> a -> b) -> [[((Anim.Size, Alignment), a)]] -> (Anim.Size, [[b]])
-- Special case to preserve shape to avoid handling it above in
-- "maximum", "transpose", etc
makeGeneric translate rows = second place $ makePlacements szAlignments
  where
    szAlignments = (map . map) fst rows
    items = (map . map) snd rows
    place rects = (zipWith . zipWith) translate rects items

make :: [[((Anim.Size, Alignment), Anim.Frame)]] -> (Anim.Size, Anim.Frame)
make = second (mconcat . concat) . makeGeneric (Anim.translate . Rect.rectTopLeft)

makeAlign :: Alignment -> [[(Anim.Size, Anim.Frame)]] -> (Anim.Size, Anim.Frame)
makeAlign alignment = make . (map . map . first) (flip (,) alignment)

makeCentered :: [[(Anim.Size, Anim.Frame)]] -> (Anim.Size, Anim.Frame)
makeCentered = makeAlign 0.5