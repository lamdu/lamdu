{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module GUI.Momentu.Align
    ( Aligned(..), alignmentRatio, value
    , boxAlign, hboxAlign, vboxAlign
    , WithTextPos(..), textTop, tValue
    , fromWithTextPos, toWithTextPos
    , TextWidget
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue(..), GluesTo, Orientation)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.State (Gui)
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget(..), R)
import qualified GUI.Momentu.Widget as Widget

import           Lamdu.Prelude

data Aligned a = Aligned
    { _alignmentRatio :: Vector2 R
    , _value :: a
    } deriving (Functor, Foldable, Traversable, Show)
Lens.makeLenses ''Aligned

data WithTextPos a = WithTextPos
    { _textTop :: R
    , _tValue :: a
    } deriving (Functor, Foldable, Traversable, Show)
Lens.makeLenses ''WithTextPos

type TextWidget f = WithTextPos (Gui Widget f)

fromWithTextPos :: SizedElement a => R -> WithTextPos a -> Aligned a
fromWithTextPos x (WithTextPos y w) =
    Aligned (Vector2 x yAlign) w
    where
        yAlign
            | y == 0 = 0 -- Avoid division which might be by zero
            | otherwise = y / w ^. Element.height

toWithTextPos :: SizedElement a => Aligned a -> WithTextPos a
toWithTextPos (Aligned (Vector2 _ yratio) w) = WithTextPos (yratio * w ^. Element.height) w

instance SizedElement a => Element (Aligned a) where
    setLayers = value . Element.setLayers
    hoverLayers = value %~ Element.hoverLayers
    empty = Aligned 0 Element.empty
    pad padding (Aligned ratio w) =
        Aligned
        { _alignmentRatio = (ratio * (w ^. Element.size) + padding) / paddedWidget ^. Element.size
        , _value = paddedWidget
        }
        where
            paddedWidget = Element.pad padding w
    assymetricPad = error "Aligned: assymetricPad not implemented"
    scale ratio = value %~ Element.scale ratio

instance SizedElement a => SizedElement (Aligned a) where size = value . Element.size

instance SizedElement a => Element (WithTextPos a) where
    setLayers = tValue . Element.setLayers
    hoverLayers = tValue %~ Element.hoverLayers
    empty = WithTextPos 0 Element.empty
    assymetricPad tl br (WithTextPos y w) =
        WithTextPos
        { _textTop = y + tl ^. _2
        , _tValue = Element.assymetricPad tl br w
        }
    scale ratio (WithTextPos y w) =
        WithTextPos
        { _textTop = y * ratio ^. _2
        , _tValue = Element.scale ratio w
        }

instance SizedElement a => SizedElement (WithTextPos a) where size = tValue . Element.size

-- Takes the alignment point of the first item.
instance ( SizedElement (Glued a b)
         , SizedElement a
         , SizedElement b, Glue a b
         ) => Glue (Aligned a) (Aligned b) where
    type Glued (Aligned a) (Aligned b) = Aligned (Glued a b)
    glue o a b =
        glueHelper fst o (a ^. absAligned) (b ^. absAligned) ^. Lens.from absAligned

instance ( SizedElement a
         , SizedElement b
         , Glue a b ) => Glue (WithTextPos a) (WithTextPos b) where
    type Glued (WithTextPos a) (WithTextPos b) = WithTextPos (Glued a b)
    -- | Vertical glue takes the top text pos
    glue o (WithTextPos ay a) (WithTextPos by b) =
        WithTextPos y glued
        where
            (Vector2 0 y, glued) =
                glueHelper fst o (Vector2 0 ay, a) (Vector2 0 by, b)

instance Glue a (Widget b) => Glue (WithTextPos a) (Widget b) where
    type Glued (WithTextPos a) (Widget b) = WithTextPos (Glued a (Widget b))
    glue o (WithTextPos y a) b = WithTextPos y (glue o a b)

instance (SizedElement (Widget a), Glue (Widget a) b) =>
         Glue (Widget a) (WithTextPos b) where
    type Glued (Widget a) (WithTextPos b) = WithTextPos (Glued (Widget a) b)
    glue o a (WithTextPos y b) =
        WithTextPos
        { _textTop =
            case o of
            Glue.Vertical -> y + a ^. Element.height
            Glue.Horizontal -> y
        , _tValue = glue o a b
        }

instance Glue a View => Glue (WithTextPos a) View where
    type Glued (WithTextPos a) View = WithTextPos (Glued a View)
    glue o (WithTextPos y a) b = WithTextPos y (glue o a b)

instance Glue View a => Glue View (WithTextPos a) where
    type Glued View (WithTextPos a) = WithTextPos (Glued View a)
    glue o a (WithTextPos y b) =
        WithTextPos
        { _textTop =
            case o of
            Glue.Vertical -> y + a ^. Element.height
            Glue.Horizontal -> y
        , _tValue = glue o a b
        }

glueHelper ::
    (Glue a b, Element b, SizedElement a) =>
    ((Vector2 R, Vector2 R) -> Vector2 R) -> Orientation ->
    (Vector2 R, a) -> (Vector2 R, b) -> (Vector2 R, Glued a b)
glueHelper chooseAlign orientation (aAbsAlign, aw) (bAbsAlign, bw) =
    ( chooseAlign
        ( aAbsAlign + max 0 aToB
        , bAbsAlign + max 0 bToA + bGlueTranslation
        )
    , glue orientation (syncAlign aToB aw) (syncAlign bToA bw)
    )
    where
        l :: Lens' (Vector2 a) a
        l = axis orientation

        -- Duplicates the logic from underlying glue:
        bGlueTranslation = 0 & l .~ aw ^. Element.size . l
        aToB = bAbsAlign - aAbsAlign & l .~ 0
        bToA = -aToB
        syncAlign move = Element.assymetricPad (max 0 move) 0

axis :: (Field1 s s a a, Field2 s s a a, Functor f) => Orientation -> (a -> f a) -> s -> f s
axis Glue.Horizontal = _1
axis Glue.Vertical = _2

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
    (SizedElement a, SizedElement b) =>
    Lens.Iso (Aligned a) (Aligned b) (AbsAligned a) (AbsAligned b)
absAligned =
    asTuple . Lens.iso (f (*)) (f fromAbs)
    where
        f op w = w & _1 %~ (`op` (w ^. _2 . Element.size))
        fromAbs align size
            | size == 0 = 0
            | otherwise = align / size

boxAlign :: (SizedElement a, GluesTo a a a) => Orientation -> Widget.R -> [a] -> a
boxAlign orientation r xs =
    Glue.box orientation (xs <&> Aligned (pure r)) ^. value

vboxAlign :: (SizedElement a, GluesTo a a a) => Widget.R -> [a] -> a
vboxAlign = boxAlign Glue.Vertical

hboxAlign :: (SizedElement a, GluesTo a a a) => Widget.R -> [a] -> a
hboxAlign = boxAlign Glue.Horizontal
