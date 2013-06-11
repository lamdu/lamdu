{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.GridView
  ( make, makeAlign, makeCentered
  , makeGeneric
  , Alignment
  , verticalAlign, vertical
  , horizontalAlign, horizontal
  ) where

import Control.Arrow ((&&&))
import Control.Lens.Operators
import Data.List (transpose)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.View (View)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect

type Alignment = Vector2 Anim.R -- ^ 0..1

makePlacements :: [[(Alignment, Anim.Size)]] -> (Anim.Size, [[(Alignment, Rect)]])
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
    colSizes = map (groupSize Lens._1) $ transpose posRows
    rowSizes = map (groupSize Lens._2) posRows
    groupSize dim group =
      (alignmentPos + maxSize snd, alignmentPos)
      where
        alignmentPos = maxSize fst
        maxSize f = maximum $ map ((^. dim) . f . snd) group
    posRows = (map . map) calcPos rows
    calcPos (alignment, size) = (size, (alignment * size, (1 - alignment) * size))

--- Displays:

-- Used by both make and Grid's make.
makeGeneric :: (Alignment -> Rect -> a -> b) -> [[(Alignment, (Anim.Size, a))]] -> (Anim.Size, [[b]])
-- Special case to preserve shape to avoid handling it above in
-- "maximum", "transpose", etc
makeGeneric translate rows =
  Lens._2 %~ place $ makePlacements szAlignments
  where
    szAlignments = (map . map) (fst &&& fst . snd) rows
    items = (map . map) (snd . snd) rows
    place aRects = (zipWith . zipWith) (uncurry translate) aRects items

make :: [[(Alignment, View)]] -> View
make = (Lens._2 %~ mconcat . concat) . makeGeneric (const (Anim.translate . (^. Rect.topLeft)))

makeAlign :: Alignment -> [[View]] -> View
makeAlign alignment =
  make . (Lens.traversed . Lens.traversed %~ (,) alignment)

makeCentered :: [[View]] -> View
makeCentered = makeAlign 0.5

vertical :: [(Alignment, View)] -> View
vertical = make . map (:[])

horizontal :: [(Alignment, View)] -> View
horizontal = make . (:[])

verticalAlign :: Alignment -> [View] -> View
verticalAlign align = vertical . map ((,) align)

horizontalAlign :: Alignment -> [View] -> View
horizontalAlign align = horizontal . map ((,) align)
