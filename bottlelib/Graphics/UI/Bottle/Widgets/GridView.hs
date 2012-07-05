{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.GridView(make, makeGeneric) where

import Control.Applicative (liftA2)
import Control.Arrow (second)
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

makePlacements :: [[Anim.Size]] -> (Anim.Size, [[Rect]])
makePlacements sizes =
  (totalSize cSizes, (zipWith . zipWith) Rect allPlacements sizes)
  where
    allPlacements = map placeColumnsInRow rowPlacements
    placeColumnsInRow y = map (`Vector2` y) columnPlacements
    rowPlacements = scanl (+) 0 (rowSizes cSizes)
    columnPlacements = scanl (+) 0 (columnSizes cSizes)
    cSizes = computeSizes sizes

--- Displays:

-- Used by both make and Grid's make.
makeGeneric :: (Rect -> a -> b) -> [[(Anim.Size, a)]] -> (Anim.Size, [[b]])
-- Special case to preserve shape to avoid handling it above in
-- "maximum", "transpose", etc
makeGeneric translate rows = second place $ makePlacements sizes
  where
    sizes = (map . map) fst rows
    items = (map . map) snd rows
    place rects = (zipWith . zipWith) translate rects items

make :: [[(Anim.Size, Anim.Frame)]] -> (Anim.Size, Anim.Frame)
make = second (mconcat . concat) . makeGeneric (Anim.translate . Rect.rectTopLeft)
