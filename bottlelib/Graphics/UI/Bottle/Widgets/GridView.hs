{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.GridView
  ( make, makeAlign, makeCentered
  , makeGeneric
  , Alignment
  ) where

import Control.Arrow (first, second)
import Data.List (transpose)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect

type Alignment = Vector2 Anim.R -- ^ 0..1

makePlacements :: [[(Anim.Size, Alignment)]] -> (Anim.Size, [[(Alignment, Rect)]])
makePlacements rows =
  (Vector2 width height, zipWith rowResult (zipWith alignPos rowPos rowSizes) posRows)
  where
    width = last colPos
    height = last rowPos
    rowPos = groupPos rowSizes
    colPos = groupPos colSizes
    alignPos pos (_, align) = pos + align
    groupPos = scanl (+) 0 . map fst
    rowResult rowSize = zipWith (itemResult rowSize) (zipWith alignPos colPos colSizes)
    itemResult alignY alignX (itemSize, (Vector2 preX preY, _)) =
      ( Vector2 (alignX / width) (alignY / height)
      , Rect (Vector2 (alignX - preX) (alignY - preY)) itemSize
      )
    colSizes = map (groupSize Vector2.fst) $ transpose posRows
    rowSizes = map (groupSize Vector2.snd) posRows
    groupSize dim group =
      (alignmentPos + maxSize snd, alignmentPos)
      where
        alignmentPos = maxSize fst
        maxSize f = maximum $ map (dim . f . snd) group
    posRows = (map . map) calcPos rows
    calcPos (size, alignment) = (size, (alignment * size, (1 - alignment) * size))

--- Displays:

-- Used by both make and Grid's make.
makeGeneric :: (Alignment -> Rect -> a -> b) -> [[((Anim.Size, Alignment), a)]] -> (Anim.Size, [[b]])
-- Special case to preserve shape to avoid handling it above in
-- "maximum", "transpose", etc
makeGeneric translate rows =
  second place $ makePlacements szAlignments
  where
    szAlignments = (map . map) fst rows
    items = (map . map) snd rows
    place aRects = (zipWith . zipWith) (uncurry translate) aRects items

make :: [[((Anim.Size, Alignment), Anim.Frame)]] -> (Anim.Size, Anim.Frame)
make = second (mconcat . concat) . makeGeneric (const (Anim.translate . Rect.rectTopLeft))

makeAlign :: Alignment -> [[(Anim.Size, Anim.Frame)]] -> (Anim.Size, Anim.Frame)
makeAlign alignment = make . (map . map . first) (flip (,) alignment)

makeCentered :: [[(Anim.Size, Anim.Frame)]] -> (Anim.Size, Anim.Frame)
makeCentered = makeAlign 0.5
