module Graphics.UI.Bottle.Widgets.GridView
  ( make, makePlacements, makeAlign, makeCentered
  , Alignment
  , verticalAlign, vertical
  , horizontalAlign, horizontal
  ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List (transpose)
import           Data.Monoid (Monoid(..))
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View

type Alignment = Vector2 Anim.R -- ^ 0..1

groupSize :: (Num a, Ord a) => Lens.Getting a b a -> [(b, b)] -> (a, a)
groupSize dim group =
  (alignmentPos + maxSize snd, alignmentPos)
  where
    alignmentPos = maxSize fst
    maxSize f = maximum $ map ((^. dim) . f) group

makePlacements ::
  [[(Alignment, View.Size, a)]] ->
  (View.Size, [[(Alignment, Rect, a)]])
makePlacements rows =
  ( Vector2 width height
  , zipWith rowResult (zipWith alignPos rowPos rowSizes) posRows
  )
  where
    width = last colPos
    height = last rowPos
    rowPos = groupPos rowSizes
    colPos = groupPos colSizes
    alignPos pos (_, align) = pos + align
    groupPos = scanl (+) 0 . map fst
    rowResult rowSize = zipWith (itemResult rowSize) (zipWith alignPos colPos colSizes)
    itemResult alignY alignX (itemSize, (Vector2 preX preY, _), a) =
      ( Vector2 (alignX / width) (alignY / height)
      , Rect (Vector2 (alignX - preX) (alignY - preY)) itemSize
      , a
      )
    colSizes = posRows & transpose <&> groupSize _1 . map (^. _2)
    rowSizes = posRows <&> groupSize _2 . map (^. _2)
    posRows = (map . map) calcPos rows
    calcPos (alignment, size, x) = (size, (alignment * size, (1 - alignment) * size), x)

--- Displays:

make :: [[(Alignment, View)]] -> View
make views =
  views
  & Lens.mapped . Lens.mapped %~ toTriplet
  & makePlacements
  & _2 %~ toFrame
  & uncurry View
  where
    toTriplet (alignment, (View size frame)) = (alignment, size, frame)
    translate (_alignment, rect, frame) =
      Anim.translate (rect ^. Rect.topLeft) frame
    toFrame placements =
      placements
      & concat
      <&> translate
      & mconcat

makeAlign :: Alignment -> [[View]] -> View
makeAlign alignment views =
  views
  & Lens.mapped . Lens.mapped %~ (,) alignment
  & make

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
