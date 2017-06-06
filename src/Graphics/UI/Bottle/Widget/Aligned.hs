{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widget.Aligned
    ( AlignedWidget, alignment, widget
    , asTuple, width
    , empty, fromCenteredWidget, fromCenteredView
    , scaleAround, scale, pad
    , hoverInPlaceOf
    , AbsAlignedWidget, absAlignedWidget
    , Orientation(..)
    , addBefore, addAfter
    , box, hbox, vbox
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.Alignment as Alignment
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.Widgets.Box (Orientation(..))

import           Lamdu.Prelude

data AlignedWidget a = AlignedWidget
    { _alignment :: Alignment
    , _widget :: Widget a
    }
Lens.makeLenses ''AlignedWidget

{-# INLINE width #-}
width :: Lens' (AlignedWidget a) Widget.R
width = widget . Widget.width

fromCenteredWidget :: Widget a -> AlignedWidget a
fromCenteredWidget = AlignedWidget 0.5

fromCenteredView :: View -> AlignedWidget a
fromCenteredView = fromCenteredWidget . Widget.fromView

empty :: AlignedWidget a
empty = fromCenteredWidget Widget.empty

-- | scale = scaleAround 0.5
--   scaleFromTopMiddle = scaleAround (Vector2 0.5 0)
scaleAround :: Alignment -> Vector2 Widget.R -> AlignedWidget a -> AlignedWidget a
scaleAround (Alignment point) ratio (AlignedWidget (Alignment align) w) =
    AlignedWidget
    { _alignment = point + (align - point) / ratio & Alignment
    , _widget = Widget.scale ratio w
    }

scale :: Vector2 Widget.R -> AlignedWidget a -> AlignedWidget a
scale ratio = widget %~ Widget.scale ratio

pad :: Vector2 Widget.R -> AlignedWidget a -> AlignedWidget a
pad padding (AlignedWidget (Alignment align) w) =
    AlignedWidget
    { _alignment =
        (align * (w ^. Widget.size) + padding) / (paddedWidget ^. Widget.size)
        & Alignment
    , _widget = paddedWidget
    }
    where
        paddedWidget = Widget.pad padding w

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf :: AlignedWidget a -> AlignedWidget a -> AlignedWidget a
layout `hoverInPlaceOf` src =
    ( srcAbsAlignment
    , layoutWidget
        & Widget.view . View.animLayers . View.layers %~ (mempty :)
        & Widget.translate (srcAbsAlignment - layoutAbsAlignment)
        & Widget.size .~ srcSize
    ) ^. Lens.from absAlignedWidget
    where
        (layoutAbsAlignment, layoutWidget) = layout ^. absAlignedWidget
        (srcAbsAlignment, srcWidget) = src ^. absAlignedWidget
        srcSize = srcWidget ^. Widget.size

{-# INLINE asTuple #-}
asTuple ::
    Lens.Iso (AlignedWidget a) (AlignedWidget b) (Alignment, Widget a) (Alignment, Widget b)
asTuple =
    Lens.iso toTup fromTup
    where
        toTup w = (w ^. alignment, w ^. widget)
        fromTup (a, w) = AlignedWidget a w

type AbsAlignedWidget a = (Vector2 Widget.R, Widget a)

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso (AlignedWidget a) (AlignedWidget b) (AbsAlignedWidget a) (AbsAlignedWidget b)
absAlignedWidget =
    asTuple . Lens.iso (f ((*) . (^. Alignment.ratio))) (f (fmap Alignment . fromAbs))
    where
        f op w = w & _1 %~ (`op` (w ^. _2 . Widget.size))
        fromAbs align size
            | size == 0 = 0
            | otherwise = align / size

axis :: Orientation -> Lens' Alignment Widget.R
axis Horizontal = _1
axis Vertical = _2

data BoxComponents a = BoxComponents
    { __widgetsBefore :: [a]
    , _focalWidget :: a
    , __widgetsAfter :: [a]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''BoxComponents

boxComponentsToWidget ::
    Orientation -> BoxComponents (AlignedWidget a) -> AlignedWidget a
boxComponentsToWidget orientation boxComponents =
    AlignedWidget
    { _alignment = boxAlign ^. focalWidget
    , _widget = boxWidget
    }
    where
        (boxAlign, boxWidget) =
            boxComponents <&> (^. asTuple)
            & Box.make orientation

addBefore :: Orientation -> [AlignedWidget a] -> AlignedWidget a -> AlignedWidget a
addBefore orientation befores layout =
    BoxComponents befores layout []
    & boxComponentsToWidget orientation
addAfter :: Orientation -> [AlignedWidget a] -> AlignedWidget a -> AlignedWidget a
addAfter orientation afters layout =
    BoxComponents [] layout afters
    & boxComponentsToWidget orientation

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box :: Orientation -> Widget.R -> [AlignedWidget a] -> AlignedWidget a
box orientation axisAlignment layouts =
    componentsFromList layouts
    & boxComponentsToWidget orientation
    & alignment . axis orientation .~ axisAlignment
    where
        componentsFromList [] = BoxComponents [] (AlignedWidget 0 Widget.empty) []
        componentsFromList (w:ws) = BoxComponents [] w ws

hbox :: Widget.R -> [AlignedWidget a] -> AlignedWidget a
hbox = box Horizontal

vbox :: Widget.R -> [AlignedWidget a] -> AlignedWidget a
vbox = box Vertical
