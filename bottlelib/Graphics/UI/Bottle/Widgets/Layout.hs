{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleInstances #-}
module Graphics.UI.Bottle.Widgets.Layout
    ( Layout
    , Box.Alignment
    , empty
    , AlignedWidget, AbsAlignedWidget
    , alignedWidget, absAlignedWidget
    , alignment, widget, width
    , fromCenteredWidget

    , AddLayout(..)

    , Orientation(..)
    , box, hbox, vbox

    , scaleAround
    , scale
    , pad
    , hoverInPlaceOf
    ) where

import           Control.Lens (Lens, Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box

import           Prelude.Compat

type AlignedWidget a = (Box.Alignment, Widget a)
type AbsAlignedWidget a = (Box.Alignment, Widget a)

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

data BoxComponents a = BoxComponents
    { _widgetsBefore :: [(Widget.R, Widget a)]
    , __focalWidget :: AlignedWidget a
    , _widgetsAfter :: [(Widget.R, Widget a)]
    }
Lens.makeLenses ''BoxComponents

boxComponentsAlignedWidgets ::
    Lens.Getting Widget.R Box.Alignment Widget.R ->
    Lens.Fold (BoxComponents a) (Widget.R, Widget a)
boxComponentsAlignedWidgets getAlign = Lens.folding $
    \(BoxComponents before (align, focal) after) ->
    before ++ [(align ^. getAlign, focal)] ++ after

data LayoutInternal a
    = LayoutSingleton (AlignedWidget a)
    | LayoutBox Orientation (BoxComponents a)

data Layout a = Layout
    { layoutInternal :: LayoutInternal a
    , toAlignedWidget :: AlignedWidget a
    }

{-# INLINE alignedWidget #-}
alignedWidget ::
    Lens.Iso (Layout a) (Layout b) (AlignedWidget a) (AlignedWidget b)
alignedWidget = Lens.iso toAlignedWidget (mkLayout . LayoutSingleton)

{-# INLINE alignment #-}
alignment :: Lens' (Layout a) Box.Alignment
alignment = alignedWidget . _1

{-# INLINE widget #-}
widget :: Lens (Layout a) (Layout b) (Widget a) (Widget b)
widget = alignedWidget . _2

{-# INLINE width #-}
width :: Lens' (Layout a) Widget.R
width = widget . Widget.width

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso (Layout a) (Layout b) (AbsAlignedWidget a) (AbsAlignedWidget b)
absAlignedWidget =
    alignedWidget . Lens.iso toAbs fromAbs
    where
        toAbs (relAlign, w) = (relAlign * w ^. Widget.size, w)
        fromAbs (absAlign, w) = (absAlign / w ^. Widget.size, w)

boxComponentsToWidget ::
    Orientation -> BoxComponents a -> AlignedWidget a
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

internalToAlignedWidget :: LayoutInternal a -> AlignedWidget a
internalToAlignedWidget (LayoutSingleton w) = w
internalToAlignedWidget (LayoutBox orientation boxComponents) =
    boxComponentsToWidget orientation boxComponents

mkLayout :: LayoutInternal a -> Layout a
mkLayout li = Layout li (internalToAlignedWidget li)

fromCenteredWidget :: Widget a -> Layout a
fromCenteredWidget w = mkLayout $ LayoutSingleton (0.5, w)

empty :: Layout a
empty = fromCenteredWidget Widget.empty

toBoxComponents :: Orientation -> Layout a -> BoxComponents a
toBoxComponents orientation layout =
    case layoutInternal layout of
    LayoutBox o boxComponents | o == orientation -> boxComponents
    _ -> BoxComponents [] (layout ^. alignedWidget) []

composeLayout ::
    Orientation ->
    (BoxComponents a -> BoxComponents a) ->
    Layout a -> Layout a
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

aligned1d :: Lens.Getting b Box.Alignment b -> Layout a -> (b, Widget a)
aligned1d getter layout = layout ^. alignedWidget & _1 %~ (^. getter)

instance align ~ Widget.R => AddLayout (align, Widget a) where
    type LayoutType (align, Widget a) = Layout a
    addBefore orientation befores =
        composeLayout orientation (widgetsBefore %~ (befores++))
    addAfter orientation afters =
        composeLayout orientation (widgetsAfter <>~ afters)

instance AddLayout (Layout a) where
    type LayoutType (Layout a) = Layout a
    addBefore orientation =
        addBefore orientation . map (aligned1d (perpAxis orientation))
    addAfter orientation =
        addAfter orientation . map (aligned1d (perpAxis orientation))

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box :: Orientation -> Widget.R -> [Layout a] -> Layout a
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

hbox :: Widget.R -> [Layout a] -> Layout a
hbox = box Horizontal

vbox :: Widget.R -> [Layout a] -> Layout a
vbox = box Vertical

-- TODO: These functions and the AlignedWidget type could possibly be
-- extracted to their own module?

-- | scale = scaleAround 0.5
--   scaleFromTopMiddle = scaleAround (Vector2 0.5 0)
scaleAround :: Vector2 Widget.R -> Vector2 Widget.R -> Layout a -> Layout a
scaleAround point ratio =
    alignedWidget %~ f
    where
        f (align, w) =
            ( point + (align - point) / ratio
            , Widget.scale ratio w
            )

scale :: Vector2 Widget.R -> Layout a -> Layout a
scale ratio = alignedWidget . _2 %~ Widget.scale ratio

pad :: Vector2 Widget.R -> Layout a -> Layout a
pad padding =
    alignedWidget %~ f
    where
        f (align, w) =
            ( align & fromRelative & (+ padding) & toRelative
            , paddedWidget
            )
            where
                paddedWidget = Widget.pad padding w
                fromRelative = (* w ^. Widget.size)
                toRelative = (/ paddedWidget ^. Widget.size)

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf :: Layout a -> Layout a -> Layout a
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
