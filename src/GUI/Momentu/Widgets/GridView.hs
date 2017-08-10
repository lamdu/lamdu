{-# LANGUAGE NoImplicitPrelude #-}
module GUI.Momentu.Widgets.GridView
    ( make, makePlacements
    ) where

import qualified Control.Lens as Lens
import           Data.Foldable (toList)
import           Data.List (transpose)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View

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
    (Traversable vert, Traversable horiz, View.SizedElement a) =>
    vert (horiz (Aligned a)) ->
    (View.Size, vert (horiz (Aligned (Rect, a))))
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
            Aligned (Vector2 alignX alignY / totalSize)
            (Rect (Vector2 (alignX - preX) (alignY - preY)) itemSize, a)
        posRowsList = toList posRows <&> toList
        colSizes = posRowsList & transpose <&> groupSize _1 . fmap (^. _2)
        rowSizes = posRowsList             <&> groupSize _2 . fmap (^. _2)
        posRows = rows <&> Lens.mapped %~ calcPos
        calcPos (Aligned alignment x) =
            (size, (alignment * size, (1 - alignment) * size), x)
            where
                size = x ^. View.size

--- Displays:

make :: (Traversable horiz, Traversable vert) => vert (horiz (Aligned View)) -> View
make views =
    makePlacements views
    & _2 %~ (^. traverse . traverse . Lens.to translate)
    & uncurry View
    where
        translate (Aligned _ (rect, view)) = View.translateLayers (rect ^. Rect.topLeft) (view ^. View.vAnimLayers)
