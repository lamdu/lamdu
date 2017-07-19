{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TemplateHaskell, RankNTypes, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Aligned
    ( Aligned(..), alignment, value
    , AlignTo(..), alignTo, alignedTo
    , hoverInPlaceOf
    , Orientation(..)
    , boxAlign, hboxAlign, vboxAlign
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import qualified Graphics.UI.Bottle.Alignment as Alignment
import           Graphics.UI.Bottle.View (Orientation)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget(..), R)
import qualified Graphics.UI.Bottle.Widget as Widget

import           Lamdu.Prelude

data Aligned a = Aligned
    { _alignment :: Alignment
    , _value :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Aligned

instance View.SetLayers a => View.SetLayers (Aligned a) where
    setLayers = value . View.setLayers

instance (View.HasSize a, View.Resizable a) => View.Resizable (Aligned a) where
    empty = Aligned 0 View.empty
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

data AlignTo a = AlignTo
    { _alignTo :: R
    , _alignedTo :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''AlignTo

instance View.HasSize a => View.HasSize (AlignTo a) where size = alignedTo . View.size

toAbsPair :: View.HasSize a => AlignTo a -> (Vector2 R, a)
toAbsPair (AlignTo xRatio xw) = (pure xRatio * xw ^. View.size, xw)

-- Takes the alignment point of the first item.
instance ( View.HasSize (View.Glued a b)
         , View.HasSize a, View.Resizable a
         , View.HasSize b, View.Resizable b
         , View.Glue a b ) => View.Glue (Aligned a) (Aligned b) where
    type Glued (Aligned a) (Aligned b) = Aligned (View.Glued a b)
    glue o a b = glueHelper fst o (a ^. absAligned) (b ^. absAligned)

instance ( View.HasSize (View.Glued a b)
         , View.HasSize a, View.Resizable a
         , View.HasSize b, View.Resizable b
         , View.Glue a b ) => View.Glue (Aligned a) (AlignTo b) where
    type Glued (Aligned a) (AlignTo b) = Aligned (View.Glued a b)
    glue o a b = glueHelper fst o (a ^. absAligned) (toAbsPair b)

instance ( View.HasSize (View.Glued a b)
         , View.HasSize a, View.Resizable a
         , View.HasSize b, View.Resizable b
         , View.Glue a b ) =>
         View.Glue (AlignTo a) (Aligned b) where
    type Glued (AlignTo a) (Aligned b) = Aligned (View.Glued a b)
    glue o a b = glueHelper snd o (toAbsPair a) (b ^. absAligned)

glueHelper ::
    ( View.Glue a b, View.Resizable a, View.Resizable b
    , View.HasSize (View.Glued a b), View.HasSize a
    ) =>
    ((Vector2 R, Vector2 R) -> Vector2 R) -> Orientation ->
    (Vector2 R, a) -> (Vector2 R, b) -> Aligned (View.Glued a b)
glueHelper chooseAlign orientation (aAbsAlign, aw) (bAbsAlign, bw) =
    ( chooseAlign
        ( aAbsAlign + max 0 aToB
        , bAbsAlign + max 0 bToA + bGlueTranslation
        )
    , View.glue orientation (syncAlign aToB aw) (syncAlign bToA bw)
    ) ^. Lens.from absAligned
    where
        l :: Lens' (Vector2 a) a
        l = View.axis orientation
        -- Duplicates the logic from underlying glue:
        bGlueTranslation = 0 & l .~ aw ^. View.size . l
        aToB = bAbsAlign - aAbsAlign & l .~ 0
        bToA = -aToB
        syncAlign move = View.assymetricPad (max 0 move) 0

-- Resize a layout to be the same alignment/size as another layout
hoverInPlaceOf ::
    (Functor f, View.HasSize a) =>
    Aligned (Widget (f Widget.EventResult)) ->
    Aligned a ->
    Aligned (Widget (f Widget.EventResult))
layout `hoverInPlaceOf` src =
    ( srcAbsAlignment
    , layoutWidget
        & Widget.translate (srcAbsAlignment - layoutAbsAlignment)
        -- We start out as layout size and resize explicitly to src
        -- size - allowing the surrounding to be corrected by the size lens:
        & Widget (layoutWidget ^. View.size)
        & View.size .~ (srcWidget ^. View.size)
        & liftLayer
    ) ^. Lens.from absAligned
    where
        liftLayer w =
            w
            & View.setLayers . View.layers %~ (mempty :)
            & Widget.mEnter . Lens._Just . Lens.mapped . Widget.enterResultLayer +~ 1
        (layoutAbsAlignment, layoutWidget) = layout ^. absAligned
        (srcAbsAlignment, srcWidget) = src ^. absAligned

{-# INLINE asTuple #-}
asTuple :: Lens.Iso (Aligned a) (Aligned b) (Alignment, a) (Alignment, b)
asTuple =
    Lens.iso toTup fromTup
    where
        toTup w = (w ^. alignment, w ^. value)
        fromTup (a, w) = Aligned a w

type AbsAligned a = (Vector2 R, a)

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

boxAlign :: (View.HasSize a, View.Resizable a, View.GluesTo a a a) => Orientation -> Widget.R -> [a] -> a
boxAlign orientation r xs =
    View.box orientation (xs <&> Aligned (Alignment (pure r))) ^. value

vboxAlign :: (View.HasSize a, View.Resizable a, View.GluesTo a a a) => Widget.R -> [a] -> a
vboxAlign = boxAlign View.Vertical

hboxAlign :: (View.HasSize a, View.Resizable a, View.GluesTo a a a) => Widget.R -> [a] -> a
hboxAlign = boxAlign View.Horizontal
