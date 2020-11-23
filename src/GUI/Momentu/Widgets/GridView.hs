module GUI.Momentu.Widgets.GridView
    ( make, makePlacements
    ) where

import qualified Control.Lens as Lens
import           Data.Foldable (toList)
import           Data.List (transpose)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View

import           GUI.Momentu.Prelude

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
    (Traversable vert, Traversable horiz, SizedElement a) =>
    vert (horiz (Aligned a)) ->
    (Element.Size, vert (horiz (Aligned (Rect, a))))
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
        alignMult = totalSize <&> \x -> if x == 0 then 1 else 1 / x
        itemResult alignY alignX (itemSize, (Vector2 preX preY, _), a) =
            Aligned (Vector2 alignX alignY * alignMult)
            (Rect (Vector2 (alignX - preX) (alignY - preY)) itemSize, a)
        posRowsList = toList posRows <&> toList
        colSizes = posRowsList & transpose <&> groupSize _1 . fmap (^. _2)
        rowSizes = posRowsList             <&> groupSize _2 . fmap (^. _2)
        posRows = rows <&> Lens.mapped %~ calcPos
        calcPos (Aligned alignment x) =
            (size, (alignment * size, (1 - alignment) * size), x)
            where
                size = x ^. Element.size

--- Displays:

make ::
    ( MonadReader env m, Has Dir.Layout env
    , Traversable horiz, Traversable vert
    ) =>
    m
    (vert (horiz (Aligned View)) ->
     (vert (horiz (Aligned ())), View))
make =
    Element.pad
    <&> \pad views ->
    let (size, placements) = makePlacements views
        translate (Aligned _ (rect, view)) =
            pad
            (rect ^. Rect.topLeft)
            (size - rect ^. Rect.bottomRight)
            view ^. View.vAnimLayers
    in  ( placements <&> Lens.mapped %~ void
        , View size (placements ^. traverse . traverse . Lens.to translate)
        )
