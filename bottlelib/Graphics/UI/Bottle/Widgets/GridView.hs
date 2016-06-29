{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, GeneralizedNewtypeDeriving, TypeSynonymInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Widgets.GridView
    ( make, makePlacements
    , Alignment(..), alignmentRatio
    , verticalAlign, vertical
    , horizontalAlign, horizontal
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List (transpose)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View

import           Prelude.Compat

newtype Alignment = Alignment { _alignmentRatio :: Vector2 Anim.R } -- ^ 0..1
    deriving (Num, Fractional)
Lens.makeLenses ''Alignment

instance Field1 Alignment Alignment Anim.R Anim.R where
    _1 = alignmentRatio . _1

instance Field2 Alignment Alignment Anim.R Anim.R where
    _2 = alignmentRatio . _2

groupSize :: Lens.Getting Anim.R b Anim.R -> [(b, b)] -> (Anim.R, Anim.R)
groupSize dim group =
    (alignmentPos + maxSize snd, alignmentPos)
    where
        alignmentPos = maxSize fst
        maxSize f = maximum $ map ((^. dim) . f) group

makePlacements ::
    [[(Alignment, View.Size, a)]] ->
    (View.Size, [[(Alignment, Rect, a)]])
makePlacements rows =
    ( totalSize
    , zipWith rowResult (zipWith alignPos rowPos rowSizes) posRows
    )
    where
        totalSize = Vector2 width height
        width = last colPos
        height = last rowPos
        rowPos = groupPos rowSizes
        colPos = groupPos colSizes
        alignPos pos (_, align) = pos + align
        groupPos = scanl (+) 0 . map fst
        rowResult rowSize =
            zipWith (itemResult rowSize) (zipWith alignPos colPos colSizes)
        itemResult alignY alignX (itemSize, (Vector2 preX preY, _), a) =
            ( Alignment (Vector2 alignX alignY / totalSize)
            , Rect (Vector2 (alignX - preX) (alignY - preY)) itemSize
            , a
            )
        colSizes = posRows & transpose <&> groupSize _1 . map (^. _2)
        rowSizes = posRows             <&> groupSize _2 . map (^. _2)
        posRows = (map . map) calcPos rows
        calcPos (Alignment alignment, size, x) =
            (size, (alignment * size, (1 - alignment) * size), x)

--- Displays:

make :: [[(Alignment, View)]] -> View
make views =
    views
    & Lens.mapped . Lens.mapped %~ toTriplet
    & makePlacements
    & _2 %~ toFrame
    & uncurry View
    where
        toTriplet (alignment, View size frame) = (alignment, size, frame)
        translate (_alignment, rect, frame) =
            Anim.translate (rect ^. Rect.topLeft) frame
        toFrame placements =
            placements
            & concat
            <&> translate
            & mconcat

vertical :: [(Alignment, View)] -> View
vertical = make . map (:[])

horizontal :: [(Alignment, View)] -> View
horizontal = make . (:[])

verticalAlign :: Alignment -> [View] -> View
verticalAlign align = vertical . map ((,) align)

horizontalAlign :: Alignment -> [View] -> View
horizontalAlign align = horizontal . map ((,) align)
