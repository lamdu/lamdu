{-# LANGUAGE TypeFamilies, TemplateHaskell, RankNTypes, FlexibleInstances #-}
module Graphics.UI.Bottle.Widgets.Layout
    ( Layout
    , Box.Alignment
    , empty
    , AlignedWidget, AbsAlignedWidget
    , alignedWidget, absAlignedWidget
    , fromCenteredWidget

    , AddLayout(..)

    , Orientation(..)
    , box, hbox, vbox
    , gridTopLeftFocal

    , scaleFromTopLeft
    , scale
    , pad
    , hoverInPlaceOf
    ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type AlignedWidget f = (Box.Alignment, Widget f)
type AbsAlignedWidget f = (Box.Alignment, Widget f)

data Orientation = Horizontal | Vertical deriving Eq

axis :: Orientation -> Lens' (Vector2 a) a
axis Horizontal = _1
axis Vertical = _2

perpAxis :: Orientation -> Lens' (Vector2 a) a
perpAxis Vertical = _1
perpAxis Horizontal = _2

boxOrientation :: Orientation -> Box.Orientation
boxOrientation Horizontal = Box.horizontal
boxOrientation Vertical = Box.vertical

data BoxComponents f = BoxComponents
    { _widgetsBefore :: [(Widget.R, Widget f)]
    , __focalWidget :: AlignedWidget f
    , _widgetsAfter :: [(Widget.R, Widget f)]
    }
Lens.makeLenses ''BoxComponents

boxComponentsAlignedWidgets ::
    Lens.Getting Widget.R Box.Alignment Widget.R ->
    Lens.Fold (BoxComponents f) (Widget.R, Widget f)
boxComponentsAlignedWidgets getAlign = Lens.folding $
    \(BoxComponents before (alignment, focal) after) ->
    before ++ [(alignment ^. getAlign, focal)] ++ after

data LayoutInternal f
    = LayoutSingleton (AlignedWidget f)
    | LayoutBox Orientation (BoxComponents f)

data Layout f = Layout
    { layoutInternal :: LayoutInternal f
    , toAlignedWidget :: AlignedWidget f
    }

{-# INLINE alignedWidget #-}
alignedWidget ::
    Lens.Iso (Layout f) (Layout g) (AlignedWidget f) (AlignedWidget g)
alignedWidget = Lens.iso toAlignedWidget (mkLayout . LayoutSingleton)

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso (Layout f) (Layout g) (AbsAlignedWidget f) (AbsAlignedWidget g)
absAlignedWidget =
    alignedWidget . Lens.iso toAbs fromAbs
    where
        toAbs (relAlign, widget) = (relAlign * widget ^. Widget.size, widget)
        fromAbs (absAlign, widget) = (absAlign / widget ^. Widget.size, widget)

boxComponentsToWidget ::
    Orientation -> BoxComponents f -> AlignedWidget f
boxComponentsToWidget orientation (BoxComponents before awidget after) =
    ( kbox ^?!
      Box.boxContent . Lens.traverse . Lens.filtered fst . _2 . Box.elementAlign
    , kbox & Box.toWidget
    )
    where
        kbox = Box.makeKeyed (boxOrientation orientation) children
        nonFocal widgets =
            widgets
            <&> _1 %~ pure
            <&> (,) False
        children =
            concat
            [ nonFocal before
            , [ awidget & (,) True ]
            , nonFocal after
            ]

internalToAlignedWidget :: LayoutInternal f -> AlignedWidget f
internalToAlignedWidget (LayoutSingleton widget) = widget
internalToAlignedWidget (LayoutBox orientation boxComponents) =
    boxComponentsToWidget orientation boxComponents

mkLayout :: LayoutInternal f -> Layout f
mkLayout li = Layout li (internalToAlignedWidget li)

fromCenteredWidget :: Widget f -> Layout f
fromCenteredWidget widget = mkLayout $ LayoutSingleton (0.5, widget)

empty :: Layout f
empty = fromCenteredWidget Widget.empty

toBoxComponents :: Orientation -> Layout f -> BoxComponents f
toBoxComponents orientation layout =
    case layoutInternal layout of
    LayoutBox o boxComponents | o == orientation -> boxComponents
    _ -> BoxComponents [] (layout ^. alignedWidget) []

composeLayout ::
    Orientation ->
    (BoxComponents f -> BoxComponents f) ->
    Layout f -> Layout f
composeLayout orientation onBoxComponents layout =
    layout
    & toBoxComponents orientation
    & onBoxComponents
    & LayoutBox orientation
    & mkLayout

class AddLayout w where
    type LayoutType w
    addBefore :: Orientation -> [w] -> LayoutType w -> LayoutType w
    addAfter :: Orientation -> [w] -> LayoutType w -> LayoutType w

aligned1d :: Lens.Getting b Box.Alignment b -> Layout f -> (b, Widget f)
aligned1d getter layout = layout ^. alignedWidget & _1 %~ (^. getter)

instance align ~ Widget.R => AddLayout (align, Widget f) where
    type LayoutType (align, Widget f) = Layout f
    addBefore orientation befores =
        composeLayout orientation (widgetsBefore %~ (befores++))
    addAfter orientation afters =
        composeLayout orientation (widgetsAfter <>~ afters)

instance AddLayout (Layout f) where
    type LayoutType (Layout f) = Layout f
    addBefore orientation =
        addBefore orientation . map (aligned1d (perpAxis orientation))
    addAfter orientation =
        addAfter orientation . map (aligned1d (perpAxis orientation))

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box :: Orientation -> Widget.R -> [Layout f] -> Layout f
box orientation axisAlignment layouts =
    layouts
    <&> toBoxComponents Horizontal
    & (^.. Lens.traverse . boxComponentsAlignedWidgets (perpAxis orientation))
    & componentsFromList
    & boxComponentsToWidget Horizontal
    & _1 . axis orientation .~ axisAlignment
    & LayoutSingleton
    & mkLayout
    where
        componentsFromList [] = BoxComponents [] (0, Widget.empty) []
        componentsFromList ((align, w):ws) = BoxComponents [] (pure align, w) ws

hbox :: Widget.R -> [Layout f] -> Layout f
hbox = box Horizontal

vbox :: Widget.R -> [Layout f] -> Layout f
vbox = box Vertical

gridTopLeftFocal :: [[Layout f]] -> Layout f
gridTopLeftFocal [] = empty
gridTopLeftFocal ([]:rs) =
    gridTopLeftFocal rs
    -- First row has 0 height, and alignment comes from top-left
    -- element:
    & alignedWidget . _1 . _2 .~ 0
gridTopLeftFocal rows@((_:_):_) =
    (topLeftAlignment, Grid.toWidget grid) ^. Lens.from alignedWidget
    where
        topLeftAlignment =
            grid ^?!
            Grid.gridContent . Lens.ix 0 . Lens.ix 0 . _2 . Grid.elementAlign
        grid =
            rows
            & Lens.mapped . Lens.mapped %~ (^. alignedWidget)
            & Grid.make

-- TODO: These functions and the AlignedWidget type could possibly be
-- extracted to their own module?

scaleFromTopLeft :: Vector2 Widget.R -> Layout f -> Layout f
scaleFromTopLeft ratio =
    alignedWidget %~ f
    where
        f (alignment, widget) = (alignment / ratio, Widget.scale ratio widget)

scale :: Vector2 Widget.R -> Layout g -> Layout g
scale ratio = alignedWidget . _2 %~ Widget.scale ratio

pad :: Vector2 Widget.R -> Layout f -> Layout f
pad padding =
    alignedWidget %~ f
    where
        f (alignment, widget) =
            ( alignment & fromRelative & (+ padding) & toRelative
            , paddedWidget
            )
            where
                paddedWidget = Widget.pad padding widget
                fromRelative = (* widget ^. Widget.size)
                toRelative = (/ paddedWidget ^. Widget.size)

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf :: Layout f -> Layout f -> Layout f
layout `hoverInPlaceOf` src =
    ( srcAbsAlignment
    , layoutWidget
        & Widget.translate (srcAbsAlignment - layoutAbsAlignment)
        & Widget.size .~ srcSize
    ) ^. Lens.from absAlignedWidget
    where
        (layoutAbsAlignment, layoutWidget) = layout ^. absAlignedWidget
        (srcAbsAlignment, srcWidget) = src ^. absAlignedWidget
        srcSize = srcWidget ^. Widget.size
