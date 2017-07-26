{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Align
    ( Aligned(..), alignmentRatio, value
    , AnchoredWidget, anchor
    , hoverInPlaceOf, hoverBesideOptions, hoverBesideOptionsAxis
    , Orientation(..)
    , boxAlign, hboxAlign, vboxAlign
    , WithTextPos(..), textTop, tValue
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Data.List.Utils (minimumOn)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.View (View, Orientation)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget(..), R, EventResult)
import qualified Graphics.UI.Bottle.Widget as Widget

import           Lamdu.Prelude

data Aligned a = Aligned
    { _alignmentRatio :: Vector2 R
    , _value :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Aligned

data AnchoredWidget a = AnchoredWidget
    { _anchorPoint :: Vector2 R
    , _anchored :: Widget a
    } deriving Functor
Lens.makeLenses ''AnchoredWidget

data WithTextPos a = WithTextPos
    { _textTop :: R
    , _tValue :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''WithTextPos

instance View.SetLayers a => View.SetLayers (Aligned a) where
    setLayers = value . View.setLayers
    hoverLayers = value %~ View.hoverLayers

instance (View.HasSize a, View.Resizable a) => View.Resizable (Aligned a) where
    empty = Aligned 0 View.empty
    pad padding (Aligned ratio w) =
        Aligned
        { _alignmentRatio = (ratio * (w ^. View.size) + padding) / paddedWidget ^. View.size
        , _value = paddedWidget
        }
        where
            paddedWidget = View.pad padding w
    assymetricPad = error "Aligned: assymetricPad not implemented"
    scale ratio = value %~ View.scale ratio

instance View.HasSize a => View.HasSize (Aligned a) where size = value . View.size

instance Widget.HasWidget AnchoredWidget where widget = anchored

instance View.SetLayers (AnchoredWidget a) where
    setLayers = anchored . View.setLayers
    hoverLayers = anchored %~ View.hoverLayers

instance Functor f => View.Resizable (AnchoredWidget (f EventResult)) where
    empty = AnchoredWidget 0 View.empty
    assymetricPad tl br (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point + tl
        , _anchored = View.assymetricPad tl br w
        }
    scale ratio (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point * ratio
        , _anchored = View.scale ratio w
        }

instance View.HasSize (AnchoredWidget a) where size = anchored . View.size

instance View.Glue (AnchoredWidget a) View where
    type Glued (AnchoredWidget a) View = AnchoredWidget a
    glue = View.glueH $ \w v -> w & View.setLayers <>~ View.hoverLayers v ^. View.vAnimLayers

instance Functor f => View.Glue View (AnchoredWidget (f EventResult)) where
    type Glued View (AnchoredWidget (f EventResult)) = AnchoredWidget (f EventResult)
    glue = View.glueH $ \v w -> w & View.setLayers <>~ View.hoverLayers v ^. View.vAnimLayers

instance Functor f => View.Glue (AnchoredWidget (f EventResult)) (Widget (f EventResult)) where
    type Glued (AnchoredWidget (f EventResult)) (Widget (f EventResult)) = AnchoredWidget (f EventResult)
    glue orientation =
        View.glueH f orientation
        where
            f (AnchoredWidget pos w0) w1 =
                AnchoredWidget pos (Widget.glueStates orientation w0 (View.hoverLayers w1))

instance Functor f => View.Glue (Widget (f EventResult)) (AnchoredWidget (f EventResult)) where
    type Glued (Widget (f EventResult)) (AnchoredWidget (f EventResult)) = AnchoredWidget (f EventResult)
    glue orientation =
        View.glueH f orientation
        where
            f w0 (AnchoredWidget pos w1) =
                AnchoredWidget pos (Widget.glueStates orientation (View.hoverLayers w0) w1)

instance View.SetLayers a => View.SetLayers (WithTextPos a) where
    setLayers = tValue . View.setLayers
    hoverLayers = tValue %~ View.hoverLayers

instance (View.HasSize a, View.Resizable a) => View.Resizable (WithTextPos a) where
    empty = WithTextPos 0 View.empty
    assymetricPad tl br (WithTextPos y w) =
        WithTextPos
        { _textTop = y + tl ^. _2
        , _tValue = View.assymetricPad tl br w
        }
    scale ratio (WithTextPos y w) =
        WithTextPos
        { _textTop = y * ratio ^. _2
        , _tValue = View.scale ratio w
        }

instance View.HasSize a => View.HasSize (WithTextPos a) where size = tValue . View.size

-- Takes the alignment point of the first item.
instance ( View.HasSize (View.Glued a b)
         , View.HasSize a, View.Resizable a
         , View.HasSize b, View.Resizable b
         , View.Glue a b ) => View.Glue (Aligned a) (Aligned b) where
    type Glued (Aligned a) (Aligned b) = Aligned (View.Glued a b)
    glue o a b =
        glueHelper fst o (a ^. absAligned) (b ^. absAligned) ^. Lens.from absAligned

instance ( View.HasSize (View.Glued a b)
         , View.HasSize a, View.Resizable a
         , View.HasSize b, View.Resizable b
         , View.Glue a b ) => View.Glue (WithTextPos a) (WithTextPos b) where
    type Glued (WithTextPos a) (WithTextPos b) = WithTextPos (View.Glued a b)
    -- | Vertical glue takes the top text pos
    glue o (WithTextPos ay a) (WithTextPos by b) =
        WithTextPos y glued
        where
            (Vector2 0 y, glued) =
                glueHelper fst o (Vector2 0 ay, a) (Vector2 0 by, b)

instance View.Glue a (Widget b) => View.Glue (WithTextPos a) (Widget b) where
    type Glued (WithTextPos a) (Widget b) = WithTextPos (View.Glued a (Widget b))
    glue o (WithTextPos y a) b = WithTextPos y (View.glue o a b)

instance (View.HasSize (Widget a), View.Glue (Widget a) b) =>
         View.Glue (Widget a) (WithTextPos b) where
    type Glued (Widget a) (WithTextPos b) = WithTextPos (View.Glued (Widget a) b)
    glue o a (WithTextPos y b) =
        WithTextPos
        { _textTop =
            case o of
            View.Vertical -> y + a ^. View.height
            View.Horizontal -> y
        , _tValue = View.glue o a b
        }

instance View.Glue a View => View.Glue (WithTextPos a) View where
    type Glued (WithTextPos a) View = WithTextPos (View.Glued a View)
    glue o (WithTextPos y a) b = WithTextPos y (View.glue o a b)

instance View.Glue View a => View.Glue View (WithTextPos a) where
    type Glued View (WithTextPos a) = WithTextPos (View.Glued View a)
    glue o a (WithTextPos y b) =
        WithTextPos
        { _textTop =
            case o of
            View.Vertical -> y + a ^. View.height
            View.Horizontal -> y
        , _tValue = View.glue o a b
        }

anchor :: Widget a -> AnchoredWidget a
anchor = AnchoredWidget 0

glueHelper ::
    (View.Glue a b, View.Resizable a, View.Resizable b, View.HasSize a) =>
    ((Vector2 R, Vector2 R) -> Vector2 R) -> Orientation ->
    (Vector2 R, a) -> (Vector2 R, b) -> (Vector2 R, View.Glued a b)
glueHelper chooseAlign orientation (aAbsAlign, aw) (bAbsAlign, bw) =
    ( chooseAlign
        ( aAbsAlign + max 0 aToB
        , bAbsAlign + max 0 bToA + bGlueTranslation
        )
    , View.glue orientation (syncAlign aToB aw) (syncAlign bToA bw)
    )
    where
        l :: Lens' (Vector2 a) a
        l = View.axis orientation
        -- Duplicates the logic from underlying glue:
        bGlueTranslation = 0 & l .~ aw ^. View.size . l
        aToB = bAbsAlign - aAbsAlign & l .~ 0
        bToA = -aToB
        syncAlign move = View.assymetricPad (max 0 move) 0

hoverBesideOptions ::
    ( View.Glue a b, View.Glue b a, View.Glued a b ~ View.Glued b a
    , View.HasSize a, View.HasSize b, View.HasSize (View.Glued a b)
    , View.Resizable a, View.Resizable b
    ) =>
    a -> b -> [View.Glued a b]
hoverBesideOptions hover src =
    do
        o <- [View.Vertical, View.Horizontal]
        hoverBesideOptionsAxis o hover src

hoverBesideOptionsAxis ::
    ( View.Glue a b, View.Glue b a, View.Glued a b ~ View.Glued b a
    , View.HasSize a, View.HasSize b, View.HasSize (View.Glued a b)
    , View.Resizable a, View.Resizable b
    ) =>
    Orientation -> a -> b -> [View.Glued a b]
hoverBesideOptionsAxis o hover src =
    do
        x <- [0, 1]
        let aSrc = Aligned x src
        let aHover = Aligned x hover
        [View.glue o aSrc aHover, View.glue o aHover aSrc] <&> (^. value)

hoverInPlaceOf ::
    Functor f =>
    [AnchoredWidget (f EventResult)] -> AnchoredWidget a -> Widget (f EventResult)
hoverInPlaceOf [] _ = error "no hover options!"
hoverInPlaceOf hoverOptions@(defaultOption:_) place
    | null focusedOptions =
        View.assymetricPad (translation defaultOption) 0 (defaultOption ^. anchored)
        & View.size .~ place ^. View.size
    | otherwise =
        Widget
        { Widget._wSize = place ^. View.size
        , Widget._wState = Widget.StateFocused makeFocused
        }
    where
        translation hover = place ^. anchorPoint - hover ^. anchorPoint
        -- All hovers *should* be same the - either focused or unfocused..
        focusedOptions =
            do
                x <- hoverOptions
                mkFocused <- x ^.. anchored . Widget.wState . Widget._StateFocused
                return (x, mkFocused)
        makeFocused surrounding =
            surrounding
            & Widget.sRight -~ sizeDiff ^. _1
            & Widget.sBottom -~ sizeDiff ^. _2
            & Widget.translateFocused (translation hover) hMakeFocused
            & Widget.fFocalAreas %~ (Rect 0 (place ^. View.size) :)
            where
                (hover, hMakeFocused) = pickOption surrounding
                sizeDiff = hover ^. View.size - place ^. View.size
        pickOption surrounding = minimumOn (negate . remainSurrouding surrounding . (^. _1)) focusedOptions
        remainSurrouding surrounding hover =
            filter (>= 0)
            [ surrounding ^. Widget.sLeft - tl ^. _1
            , surrounding ^. Widget.sTop - tl ^. _2
            , surrounding ^. Widget.sRight - br ^. _1
            , surrounding ^. Widget.sBottom - br ^. _2
            ]
            & length
            where
                tl = negate (translation hover)
                br = hover ^. View.size - place ^. View.size - tl

{-# INLINE asTuple #-}
asTuple :: Lens.Iso (Aligned a) (Aligned b) (Vector2 R, a) (Vector2 R, b)
asTuple =
    Lens.iso toTup fromTup
    where
        toTup w = (w ^. alignmentRatio, w ^. value)
        fromTup (a, w) = Aligned a w

type AbsAligned a = (Vector2 R, a)

{-# INLINE absAligned #-}
absAligned ::
    (View.HasSize a, View.HasSize b) =>
    Lens.Iso (Aligned a) (Aligned b) (AbsAligned a) (AbsAligned b)
absAligned =
    asTuple . Lens.iso (f (*)) (f fromAbs)
    where
        f op w = w & _1 %~ (`op` (w ^. _2 . View.size))
        fromAbs align size
            | size == 0 = 0
            | otherwise = align / size

boxAlign :: (View.HasSize a, View.Resizable a, View.GluesTo a a a) => Orientation -> Widget.R -> [a] -> a
boxAlign orientation r xs =
    View.box orientation (xs <&> Aligned (pure r)) ^. value

vboxAlign :: (View.HasSize a, View.Resizable a, View.GluesTo a a a) => Widget.R -> [a] -> a
vboxAlign = boxAlign View.Vertical

hboxAlign :: (View.HasSize a, View.Resizable a, View.GluesTo a a a) => Widget.R -> [a] -> a
hboxAlign = boxAlign View.Horizontal
