{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}
module Graphics.UI.Bottle.Widget.Aligned
    ( AlignedWidget(..), alignment, aWidget
    , empty, fromView
    , scaleAround, scale
    , hoverInPlaceOf
    , AbsAlignedWidget, absAlignedWidget
    , Orientation(..)
    , addBefore, addAfter
    , box, hbox, vbox
    , boxWithViews
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.Alignment as Alignment
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.Widgets.Box (Orientation(..))

import           Lamdu.Prelude

data AlignedWidget a = AlignedWidget
    { _alignment :: Alignment
    , _aWidget :: Widget a
    } deriving Functor
Lens.makeLenses ''AlignedWidget

instance View.MkView (AlignedWidget a) where setView = aWidget . View.setView
instance Functor f => View.Pad (AlignedWidget (f Widget.EventResult)) where
    pad padding (AlignedWidget (Alignment align) w) =
        AlignedWidget
        { _alignment =
            (align * (w ^. View.size) + padding) / (paddedWidget ^. View.size)
            & Alignment
        , _aWidget = paddedWidget
        }
        where
            paddedWidget = View.pad padding w

instance View.HasSize (AlignedWidget a) where size = aWidget . View.size
instance E.HasEventMap AlignedWidget where eventMap = aWidget . E.eventMap
instance Widget.HasWidget AlignedWidget where widget = aWidget

empty :: AlignedWidget a
empty = AlignedWidget 0 Widget.empty

fromView :: Alignment -> View -> AlignedWidget a
fromView x = AlignedWidget x . Widget.fromView

-- | scale = scaleAround 0.5
--   scaleFromTopMiddle = scaleAround (Vector2 0.5 0)
scaleAround ::
    Functor f => Alignment -> Vector2 Widget.R ->
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult)
scaleAround (Alignment point) ratio (AlignedWidget (Alignment align) w) =
    AlignedWidget
    { _alignment = point + (align - point) / ratio & Alignment
    , _aWidget = Widget.scale ratio w
    }

scale ::
    Functor f => Vector2 Widget.R ->
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult)
scale ratio = aWidget %~ Widget.scale ratio

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf ::
    Functor f =>
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult)
layout `hoverInPlaceOf` src =
    ( srcAbsAlignment
    , layoutWidget
        & View.setView . View.vMakeLayers . Lens.mapped . View.layers %~ (mempty :)
        & Widget.translate (srcAbsAlignment - layoutAbsAlignment)
        & View.size .~ srcSize
    ) ^. Lens.from absAlignedWidget
    where
        (layoutAbsAlignment, layoutWidget) = layout ^. absAlignedWidget
        (srcAbsAlignment, srcWidget) = src ^. absAlignedWidget
        srcSize = srcWidget ^. View.size

{-# INLINE asTuple #-}
asTuple ::
    Lens.Iso (AlignedWidget a) (AlignedWidget b) (Alignment, Widget a) (Alignment, Widget b)
asTuple =
    Lens.iso toTup fromTup
    where
        toTup w = (w ^. alignment, w ^. aWidget)
        fromTup (a, w) = AlignedWidget a w

type AbsAlignedWidget a = (Vector2 Widget.R, Widget a)

{-# INLINE absAlignedWidget #-}
absAlignedWidget ::
    Lens.Iso (AlignedWidget a) (AlignedWidget b) (AbsAlignedWidget a) (AbsAlignedWidget b)
absAlignedWidget =
    asTuple . Lens.iso (f ((*) . (^. Alignment.ratio))) (f (fmap Alignment . fromAbs))
    where
        f op w = w & _1 %~ (`op` (w ^. _2 . View.size))
        fromAbs align size
            | size == 0 = 0
            | otherwise = align / size

axis :: Orientation -> Lens' Alignment Widget.R
axis Horizontal = _1
axis Vertical = _2

data BoxComponents a = BoxComponents
    { __aWidgetsBefore :: [a]
    , _focalWidget :: a
    , __aWidgetsAfter :: [a]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''BoxComponents

boxComponentsToWidget ::
    Functor f => Orientation ->
    BoxComponents (AlignedWidget (f Widget.EventResult)) ->
    AlignedWidget (f Widget.EventResult)
boxComponentsToWidget orientation boxComponents =
    AlignedWidget
    { _alignment = boxAlign ^. focalWidget
    , _aWidget = boxWidget
    }
    where
        (boxAlign, boxWidget) =
            boxComponents <&> (^. asTuple)
            & Box.make orientation

addBefore ::
    Functor f => Orientation ->
    [AlignedWidget (f Widget.EventResult)] ->
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult)
addBefore orientation befores layout =
    BoxComponents befores layout []
    & boxComponentsToWidget orientation
addAfter ::
    Functor f => Orientation ->
    [AlignedWidget (f Widget.EventResult)] ->
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult)
addAfter orientation afters layout =
    BoxComponents [] layout afters
    & boxComponentsToWidget orientation

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box ::
    Functor f => Orientation -> Widget.R ->
    [AlignedWidget (f Widget.EventResult)] ->
    AlignedWidget (f Widget.EventResult)
box orientation axisAlignment layouts =
    componentsFromList layouts
    & boxComponentsToWidget orientation
    & alignment . axis orientation .~ axisAlignment
    where
        componentsFromList [] = BoxComponents [] (AlignedWidget 0 Widget.empty) []
        componentsFromList (w:ws) = BoxComponents [] w ws

hbox ::
    Functor f => Widget.R ->
    [AlignedWidget (f Widget.EventResult)] ->
    AlignedWidget (f Widget.EventResult)
hbox = box Horizontal

vbox ::
    Functor f => Widget.R ->
    [AlignedWidget (f Widget.EventResult)] ->
    AlignedWidget (f Widget.EventResult)
vbox = box Vertical

boxWithViews ::
    Functor f =>
    Orientation -> [(Widget.R, View)] -> [(Widget.R, View)] ->
    AlignedWidget (f Widget.EventResult) ->
    AlignedWidget (f Widget.EventResult)
boxWithViews orientation befores afters w =
    AlignedWidget
    { _alignment = resultAlignment
    , _aWidget =
        w ^. aWidget
        & Widget.translate (wRect ^. Rect.topLeft)
        & View.size .~ size
        & View.setView . View.vMakeLayers <>~ layers beforesPlacements <> layers aftersPlacements
    }
    where
        toTriplet (align, view) = (align, view ^. View.size, view)
        toAlignment x = Alignment (Vector2 x x)
        (size, BoxComponents beforesPlacements (resultAlignment, wRect, _v) aftersPlacements) =
            BoxComponents
                (befores <&> _1 %~ toAlignment)
                (w ^. alignment, w ^. aWidget . Widget.wView)
                (afters <&> _1 %~ toAlignment)
            <&> toTriplet
            & Box.makePlacements orientation
        layers placements = placements <&> translateView & mconcat
        translateView (_alignment, rect, view) =
            View.translateMakeLayers (rect ^. Rect.topLeft) (view ^. View.vMakeLayers)
