{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances #-}
module Graphics.UI.Bottle.Aligned
    ( Aligned(..), alignment, value
    , fromView
    , hoverInPlaceOf
    , Orientation(..)
    , addBefore, addAfter
    , box, hbox, vbox
    , boxWithViews
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.Alignment as Alignment
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.Widgets.Box (Orientation(..))

import           Lamdu.Prelude

data Aligned a = Aligned
    { _alignment :: Alignment
    , _value :: a
    } deriving Functor
Lens.makeLenses ''Aligned

instance View.SetLayers a => View.SetLayers (Aligned a) where setLayers = value . View.setLayers
instance (View.HasSize a, View.Resizable a) => View.Resizable (Aligned a) where
    pad padding (Aligned (Alignment align) w) =
        Aligned
        { _alignment =
            (align * (w ^. View.size) + padding) / (paddedWidget ^. View.size)
            & Alignment
        , _value = paddedWidget
        }
        where
            paddedWidget = View.pad padding w
    assymetricPad = error "Aligned: assymetricPad not implemented"
    scale ratio = value %~ View.scale ratio

instance View.HasSize a => View.HasSize (Aligned a) where size = value . View.size

-- TODO: Remove
fromView :: Alignment -> View -> Aligned (Widget a)
fromView x = Aligned x . Widget.fromView

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf ::
    Functor f =>
    Aligned (Widget (f Widget.EventResult)) ->
    Aligned (Widget (f Widget.EventResult)) ->
    Aligned (Widget (f Widget.EventResult))
layout `hoverInPlaceOf` src =
    ( srcAbsAlignment
    , layoutWidget
        & View.setLayers . View.layers %~ (mempty :)
        & Widget.mEnter . Lens._Just . Lens.mapped . Widget.enterResultLayer +~ 1
        & Widget.translate (srcAbsAlignment - layoutAbsAlignment)
        & View.size .~ srcSize
    ) ^. Lens.from absAligned
    where
        (layoutAbsAlignment, layoutWidget) = layout ^. absAligned
        (srcAbsAlignment, srcWidget) = src ^. absAligned
        srcSize = srcWidget ^. View.size

{-# INLINE asTuple #-}
asTuple :: Lens.Iso (Aligned a) (Aligned b) (Alignment, a) (Alignment, b)
asTuple =
    Lens.iso toTup fromTup
    where
        toTup w = (w ^. alignment, w ^. value)
        fromTup (a, w) = Aligned a w

type AbsAligned a = (Vector2 Widget.R, a)

{-# INLINE absAligned #-}
absAligned ::
    (View.HasSize a, View.HasSize b) =>
    Lens.Iso (Aligned a) (Aligned b) (AbsAligned a) (AbsAligned b)
absAligned =
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
    { __before :: [a]
    , _focalWidget :: a
    , __after :: [a]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''BoxComponents

boxComponentsToWidget ::
    Functor f => Orientation ->
    BoxComponents (Aligned (Widget (f Widget.EventResult))) ->
    Aligned (Widget (f Widget.EventResult))
boxComponentsToWidget orientation boxComponents =
    Aligned
    { _alignment = boxAlign ^. focalWidget
    , _value = boxWidget
    }
    where
        (boxAlign, boxWidget) =
            boxComponents <&> (^. asTuple)
            & Box.make orientation

addBefore ::
    Functor f => Orientation ->
    [Aligned (Widget (f Widget.EventResult))] ->
    Aligned (Widget (f Widget.EventResult)) ->
    Aligned (Widget (f Widget.EventResult))
addBefore orientation befores layout =
    BoxComponents befores layout []
    & boxComponentsToWidget orientation
addAfter ::
    Functor f => Orientation ->
    [Aligned (Widget (f Widget.EventResult))] ->
    Aligned (Widget (f Widget.EventResult)) ->
    Aligned (Widget (f Widget.EventResult))
addAfter orientation afters layout =
    BoxComponents [] layout afters
    & boxComponentsToWidget orientation

-- The axisAlignment is the alignment point to choose within the resulting box
-- i.e: Horizontal box -> choose eventual horizontal alignment point
box ::
    Functor f => Orientation -> Widget.R ->
    [Aligned (Widget (f Widget.EventResult))] ->
    Aligned (Widget (f Widget.EventResult))
box orientation axisAlignment layouts =
    componentsFromList layouts
    & boxComponentsToWidget orientation
    & alignment . axis orientation .~ axisAlignment
    where
        componentsFromList [] = BoxComponents [] (Aligned 0 Widget.empty) []
        componentsFromList (w:ws) = BoxComponents [] w ws

hbox ::
    Functor f => Widget.R ->
    [Aligned (Widget (f Widget.EventResult))] ->
    Aligned (Widget (f Widget.EventResult))
hbox = box Horizontal

vbox ::
    Functor f => Widget.R ->
    [Aligned (Widget (f Widget.EventResult))] ->
    Aligned (Widget (f Widget.EventResult))
vbox = box Vertical

boxWithViews ::
    Functor f =>
    Orientation -> [(Widget.R, View)] -> [(Widget.R, View)] ->
    Aligned (Widget (f Widget.EventResult)) ->
    Aligned (Widget (f Widget.EventResult))
boxWithViews orientation befores afters w =
    Aligned
    { _alignment = resultAlignment
    , _value =
        w ^. value
        & Widget.translate (wRect ^. Rect.topLeft)
        & View.size .~ size
        & View.setLayers <>~ layers beforesPlacements <> layers aftersPlacements
    }
    where
        toTriplet (align, view) = (align, view ^. View.size, view)
        toAlignment x = Alignment (Vector2 x x)
        (size, BoxComponents beforesPlacements (resultAlignment, wRect, _v) aftersPlacements) =
            BoxComponents
                (befores <&> _1 %~ toAlignment)
                (w ^. alignment, w ^. value . Widget.wView)
                (afters <&> _1 %~ toAlignment)
            <&> toTriplet
            & Box.makePlacements orientation
        layers placements = placements <&> translateView & mconcat
        translateView (_alignment, rect, view) =
            View.translateLayers (rect ^. Rect.topLeft) (view ^. View.vAnimLayers)
