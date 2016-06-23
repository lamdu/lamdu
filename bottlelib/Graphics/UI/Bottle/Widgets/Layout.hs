{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Layout
    ( Layout
    , Box.Alignment
    , empty
    , AbsAlignedWidget
    , absAlignedWidget
    , alignment, widget, width
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
import           Data.Traversable.Generalized (GTraversable(..))
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Widget (Widget, WidgetF(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box

import           Prelude.Compat

data WithAlignment a = WithAlignment
    { _wAlignment :: Box.Alignment
    , _wValue :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''WithAlignment

instance GTraversable WithAlignment where
    gTraverse = wValue

type Layout a = WidgetF WithAlignment a
type AbsAlignedWidget a = Layout a

data Orientation = Horizontal | Vertical deriving Eq

axis :: Orientation -> Lens' (Vector2 a) a
axis Horizontal = _1
axis Vertical = _2

boxOrientation :: Orientation -> Box.Orientation
boxOrientation Horizontal = Box.horizontal
boxOrientation Vertical = Box.vertical

data BoxComponents a = BoxComponents
    { _widgetsBefore :: [Layout a]
    , _focalWidget :: Layout a
    , _widgetsAfter :: [Layout a]
    }

{-# INLINE alignment #-}
alignment :: Lens' (Layout a) Box.Alignment
alignment = Widget.sequenced . wAlignment
-- alignment f (Widget.WidgetFocused x) = wAlignment f x <&> Widget.WidgetFocused
-- alignment f (Widget.WidgetNotFocused x) = wAlignment f x <&> Widget.WidgetNotFocused

{-# INLINE widget #-}
widget :: Lens (Layout a) (Layout b) (Widget a) (Widget b)
widget = Widget.sequenced . wValue

{-# INLINE width #-}
width :: Lens' (Layout a) Widget.R
width = widget . Widget.width

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso (Layout a) (Layout b) (AbsAlignedWidget a) (AbsAlignedWidget b)
absAlignedWidget =
    Lens.iso (Widget.hoist (f (*))) (Widget.hoist (f (flip (/))))
    where
        f op w = w & wAlignment %~ op (w ^. wValue . Widget.size)

boxComponentsToWidget ::
    Orientation -> BoxComponents a -> Layout a
boxComponentsToWidget orientation (BoxComponents before awidget after) =
    Box.toWidget kbox
    & Widget.hoist (WithAlignment align . (^. Lens._Wrapped))
    where
        align =
            kbox ^?!
            Box.boxContent . Lens.traverse . Lens.filtered fst . _2 . Box.elementAlign
        kbox =
            children <&> Lens._2 %~ toTuple . (^. Widget.sequenced)
            & Box.makeKeyed (boxOrientation orientation)
        toTuple w = (w ^. wAlignment, w ^. wValue)
        children =
            concat
            [ before <&> (,) False
            , [ awidget & (,) True ]
            , after <&> (,) False
            ]

fromCenteredWidget :: Widget a -> Layout a
fromCenteredWidget = Widget.hoist (WithAlignment 0.5 . (^. Lens._Wrapped))

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
scaleAround :: Vector2 Widget.R -> Vector2 Widget.R -> Layout a -> Layout a
scaleAround point ratio w =
    Widget.scale ratio w
    & alignment .~ point + (w ^. alignment - point) / ratio

pad :: Vector2 Widget.R -> Layout a -> Layout a
pad padding =
    absAlignedWidget %~ f
    where
        f w =
            w
            & Widget.sequenced . wValue %~ Widget.pad padding
            & alignment +~ padding

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf :: Layout a -> Layout a -> Layout a
layout `hoverInPlaceOf` src =
    layout
    & Widget.sequenced . wValue %~
        Widget.translate (srcAlign - layout ^. absAlignedWidget . alignment)
    & Widget.size .~ (src ^. Widget.size)
    & absAlignedWidget . alignment .~ srcAlign
    where
        srcAlign = src ^. absAlignedWidget . alignment
