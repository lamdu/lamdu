{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Layout
    ( Layout
    , Box.Alignment
    , empty
    , AbsAlignedWidget
    , absAlignedWidget
    , alignment, widget
    , fromCenteredWidget

    , addBefore, addAfter

    , Orientation(..)
    , box, hbox, vbox

    , scaleAround
    , pad
    , hoverInPlaceOf
    ) where

import qualified Control.Lens as Lens
import           Control.Lens (Lens, Lens')
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Widget (Widget, WidgetF(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.Widgets.Box (Orientation(..))

import           Prelude.Compat

type Layout = WidgetF ((,) Box.Alignment)
type AbsAlignedWidget = WidgetF ((,) (Vector2 Widget.R))

axis :: Orientation -> Lens' Box.Alignment Widget.R
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
    Lens.Iso (Layout a) (Layout b) (AbsAlignedWidget a) (AbsAlignedWidget b)
absAlignedWidget =
    Lens.iso (f ((*) . (^. Box.alignmentRatio))) (f (fmap Box.Alignment . (/)))
    where
        f op w = w & alignment %~ (`op` (w ^. Widget.size))

boxComponentsToWidget ::
    Orientation -> BoxComponents (Layout a) -> Layout a
boxComponentsToWidget orientation boxComponents =
    Widget.hoist ((,) align . (^. Lens._Wrapped)) boxWidget
    where
        align = boxAlign ^. focalWidget
        (boxAlign, boxWidget) =
            boxComponents <&> (^. Widget.sequenced)
            & Box.make orientation

fromCenteredWidget :: Widget a -> Layout a
fromCenteredWidget = Widget.hoist ((,) 0.5 . (^. Lens._Wrapped))

empty :: Layout a
empty = fromCenteredWidget Widget.empty

addBefore :: Orientation -> [Layout a] -> Layout a -> Layout a
addBefore orientation befores layout =
    BoxComponents befores layout []
    & boxComponentsToWidget orientation
addAfter :: Orientation -> [Layout a] -> Layout a -> Layout a
addAfter orientation afters layout =
    BoxComponents [] layout afters
    & boxComponentsToWidget orientation

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box :: Orientation -> Widget.R -> [Layout a] -> Layout a
box orientation axisAlignment layouts =
    componentsFromList layouts
    & boxComponentsToWidget orientation
    & alignment . axis orientation .~ axisAlignment
    where
        componentsFromList [] = BoxComponents [] empty []
        componentsFromList (w:ws) = BoxComponents [] w ws

hbox :: Widget.R -> [Layout a] -> Layout a
hbox = box Horizontal

vbox :: Widget.R -> [Layout a] -> Layout a
vbox = box Vertical

-- | scale = scaleAround 0.5
--   scaleFromTopMiddle = scaleAround (Vector2 0.5 0)
scaleAround :: Box.Alignment -> Vector2 Widget.R -> Layout a -> Layout a
scaleAround point ratio w =
    Widget.scale ratio w
    & alignment .~ point + ((w ^. alignment - point) & Box.alignmentRatio //~ ratio)

pad :: Vector2 Widget.R -> Layout a -> Layout a
pad padding =
    absAlignedWidget %~ f
    where
        f w =
            w
            & widget %~ Widget.pad padding
            & alignment +~ padding

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf :: Layout a -> Layout a -> Layout a
layout `hoverInPlaceOf` src =
    layout
    & Widget.onWidgetData
        (Widget.translate (srcAlign - layout ^. absAlignedWidget . alignment))
    & Widget.size .~ (src ^. Widget.size)
    & absAlignedWidget . alignment .~ srcAlign
    where
        srcAlign = src ^. absAlignedWidget . alignment
