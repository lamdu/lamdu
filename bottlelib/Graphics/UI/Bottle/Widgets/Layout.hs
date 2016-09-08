{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Layout
    ( Alignment
    , empty
    , AbsAlignedWidget
    , absAlignedWidget
    , alignment, widget
    , fromCenteredWidget

    , addBefore, addAfter

    , Orientation(..)
    , box, hbox, vbox

    , scaleAround, scale
    , pad, assymetricPad
    , hoverInPlaceOf, hoverAt
    ) where

import qualified Control.Lens as Lens
import           Control.Lens (Lens, Lens')
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Alignment as Alignment
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget, WidgetF(..), WidgetData)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.Widgets.Box (Orientation(..))

import           Prelude.Compat

type AbsAlignedWidget = WidgetF ((,) (Vector2 Widget.R))

axis :: Orientation -> Lens' Alignment Widget.R
axis Horizontal = _1
axis Vertical = _2

data BoxComponents a = BoxComponents
    { __widgetsBefore :: [a]
    , _focalWidget :: a
    , __widgetsAfter :: [a]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''BoxComponents

{-# INLINE alignment #-}
alignment ::
    Lens
    (WidgetF ((,) a) x)
    (WidgetF ((,) b) x)
    a b
alignment f = Widget.widgetF (_1 f)

{-# INLINE widget #-}
widget ::
    Lens
    (WidgetF ((,) alignment) a)
    (WidgetF ((,) alignment) b)
    (Widget a) (Widget b)
widget = Widget.sequenced . _2

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso
    (Alignment, WidgetData fo a)
    (Alignment, WidgetData fo b)
    (Vector2 Widget.R, WidgetData fo a)
    (Vector2 Widget.R, WidgetData fo b)
absAlignedWidget =
    Lens.iso (f ((*) . (^. Alignment.ratio))) (f (fmap Alignment . (/)))
    where
        f op (align, wd) = (op align (wd ^. Widget.wView . View.size), wd)

boxComponentsToWidget :: Orientation -> BoxComponents (WidgetF ((,) Alignment) a) -> WidgetF ((,) Alignment) a
boxComponentsToWidget orientation boxComponents =
    Widget.hoist ((,) align . (^. Lens._Wrapped)) boxWidget
    where
        align = boxAlign ^. focalWidget
        (boxAlign, boxWidget) = Box.make orientation boxComponents

fromCenteredWidget :: Widget a -> WidgetF ((,) Alignment) a
fromCenteredWidget = Widget.hoist ((,) 0.5 . (^. Lens._Wrapped))

empty :: WidgetF ((,) Alignment) a
empty = fromCenteredWidget Widget.empty

addBefore :: Orientation -> [WidgetF ((,) Alignment) a] -> WidgetF ((,) Alignment) a -> WidgetF ((,) Alignment) a
addBefore orientation befores layout =
    BoxComponents befores layout []
    & boxComponentsToWidget orientation
addAfter :: Orientation -> [WidgetF ((,) Alignment) a] -> WidgetF ((,) Alignment) a -> WidgetF ((,) Alignment) a
addAfter orientation afters layout =
    BoxComponents [] layout afters
    & boxComponentsToWidget orientation

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box :: Orientation -> Widget.R -> [WidgetF ((,) Alignment) a] -> WidgetF ((,) Alignment) a
box orientation axisAlignment layouts =
    componentsFromList layouts
    & boxComponentsToWidget orientation
    & alignment . axis orientation .~ axisAlignment
    where
        componentsFromList [] = BoxComponents [] empty []
        componentsFromList (w:ws) = BoxComponents [] w ws

hbox :: Widget.R -> [WidgetF ((,) Alignment) a] -> WidgetF ((,) Alignment) a
hbox = box Horizontal

vbox :: Widget.R -> [WidgetF ((,) Alignment) a] -> WidgetF ((,) Alignment) a
vbox = box Vertical

-- | scale = scaleAround 0.5 (modulu efficiency)
--   scaleFromTopMiddle = scaleAround (Vector2 0.5 0)
scaleAround ::
    Alignment -> Vector2 Widget.R ->
    (Alignment, WidgetData t a) ->
    (Alignment, WidgetData t a)
scaleAround point ratio (align, w) =
    ( point + ((align - point) & Alignment.ratio //~ ratio)
    , Widget.scale ratio w
    )

-- | More efficient special-case of scale around center
scale :: Vector2 Widget.R -> WidgetF ((,) Alignment) a -> WidgetF ((,) Alignment) a
scale ratio = Widget.onWidgetData (Widget.scale ratio)

pad ::
    Vector2 Widget.R ->
    (Alignment, WidgetData fo a) -> (Alignment, WidgetData fo a)
pad padding = assymetricPad padding padding

assymetricPad ::
    Vector2 Widget.R -> Vector2 Widget.R ->
    (Alignment, WidgetData fo a) -> (Alignment, WidgetData fo a)
assymetricPad leftAndTop rightAndBottom =
    absAlignedWidget %~ f
    where
        f (p, wd) =
            ( p + leftAndTop
            , Widget.assymetricPad leftAndTop rightAndBottom wd
            )

hoverAt ::
    (Alignment, WidgetData fo b) -> (Alignment, Widget.Size) ->
    (Alignment, WidgetData fo b)
layout `hoverAt` (srcAlign, srcSize) =
    layout
    & _2 %~ Widget.translate (srcAbsAlign - layout ^. absAlignedWidget . _1)
    & _2 %~ Widget.wView . View.size .~ srcSize
    & absAlignedWidget . _1 .~ srcAbsAlign
    where
        srcAbsAlign = srcAlign ^. Alignment.ratio * srcSize

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf ::
    (Alignment, WidgetData fo b) -> WidgetF ((,) Alignment) a ->
    (Alignment, WidgetData fo b)
layout `hoverInPlaceOf` w =
    layout `hoverAt` (w ^. alignment, w ^. Widget.size)
