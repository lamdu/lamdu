{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Widgets.GridView
    ( make, makePlacements
    , Alignment(..)
    , verticalAlign, vertical
    , horizontalAlign, horizontal
    ) where

import qualified Control.Lens as Lens
import           Data.Foldable (toList)
import           Data.List (transpose)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View

import           Lamdu.Prelude

groupSize ::
    (Functor f, Foldable f) =>
    Lens.Getting Anim.R b Anim.R -> f (b, b) -> (Anim.R, Anim.R)
groupSize dim group =
    (alignmentPos + maxSize snd, alignmentPos)
    where
        alignmentPos = maxSize fst
        maxSize f = group <&> f <&> (^. dim) & maximum

traverseList :: Traversable t => Lens (t a) (t b) [a] [b]
traverseList = Lens.unsafePartsOf traverse

makePlacements ::
    (Traversable vert, Traversable horiz, View.HasSize a) =>
    vert (horiz (Alignment, a)) ->
    (View.Size, vert (horiz (Alignment, Rect, a)))
makePlacements rows =
    ( totalSize
    , posRows
      & traverseList %~ zipWith rowResult rowAlignments
    )
    where
        rowAlignments =
            rowSizes & traverseList %~ zipWith alignPos rowPos
        totalSize = Vector2 width height
        width = last colPos
        height = last rowPos
        rowPos = groupPos rowSizes
        colPos = groupPos colSizes
        alignPos pos (_, align) = pos + align
        groupPos x = x <&> fst & scanl (+) 0
        rowResult rowSize =
            traverseList %~
            zipWith (itemResult rowSize)
            ( colSizes
              & traverseList %~ zipWith alignPos colPos
            )
        itemResult alignY alignX (itemSize, (Vector2 preX preY, _), a) =
            ( Alignment (Vector2 alignX alignY / totalSize)
            , Rect (Vector2 (alignX - preX) (alignY - preY)) itemSize
            , a
            )
        posRowsList = posRows & toList <&> toList
        colSizes = posRowsList & transpose <&> groupSize _1 . fmap (^. _2)
        rowSizes = posRowsList             <&> groupSize _2 . fmap (^. _2)
        posRows = (fmap . fmap) calcPos rows
        calcPos (Alignment alignment, x) =
            (size, (alignment * size, (1 - alignment) * size), x)
            where
                size = x ^. View.size

--- Displays:

make ::
    (Traversable horiz, Traversable vert) =>
    vert (horiz (Alignment, View)) -> View
make views =
    makePlacements views
    & _2 %~ (^. traverse . traverse . Lens.to translate)
    & uncurry View
    where
        translate (_alignment, rect, view) = View.translateLayers (rect ^. Rect.topLeft) (view ^. View.vAnimLayers)

vertical :: [(Alignment, View)] -> View
vertical = make . map (:[])

horizontal :: [(Alignment, View)] -> View
horizontal = make . (:[])

verticalAlign :: Alignment -> [View] -> View
verticalAlign align = vertical . map ((,) align)

horizontalAlign :: Alignment -> [View] -> View
horizontalAlign align = horizontal . map ((,) align)
