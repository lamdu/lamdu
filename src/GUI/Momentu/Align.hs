{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module GUI.Momentu.Align
    ( Aligned(..), alignmentRatio, value
    , boxAlign, hboxAlign, vboxAlign
    , WithTextPos(..), textTop, tValue
    , fromWithTextPos, toWithTextPos
    , TextWidget
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue(..), GluesTo)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget(..), R)
import qualified GUI.Momentu.Widget as Widget

import           GUI.Momentu.Prelude

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

type TextWidget f = WithTextPos (Widget f)

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
    setLayeredImage = value . Element.setLayeredImage
    hoverLayeredImage = value %~ Element.hoverLayeredImage
    empty = Aligned 0 Element.empty
    padImpl topLeftPadding bottomRightPadding (Aligned ratio w) =
        Aligned
        { _alignmentRatio =
            (ratio * w ^. Element.size + topLeftPadding) / paddedWidget ^. Element.size
        , _value = paddedWidget
        }
        where
            paddedWidget = Element.padImpl topLeftPadding bottomRightPadding w
    scale ratio = value %~ Element.scale ratio

instance SizedElement a => SizedElement (Aligned a) where size = value . Element.size

instance SizedElement a => Element (WithTextPos a) where
    setLayeredImage = tValue . Element.setLayeredImage
    hoverLayeredImage = tValue %~ Element.hoverLayeredImage
    empty = WithTextPos 0 Element.empty
    padImpl tl br (WithTextPos y w) =
        WithTextPos
        { _textTop = y + tl ^. _2
        , _tValue = Element.padImpl tl br w
        }
    scale ratio (WithTextPos y w) =
        WithTextPos
        { _textTop = y * ratio ^. _2
        , _tValue = Element.scale ratio w
        }

instance SizedElement a => SizedElement (WithTextPos a) where size = tValue . Element.size

-- Takes the alignment point of the first item.
instance
    ( SizedElement (Glued a b)
    , SizedElement a, SizedElement b
    , Glue env a b, Has Dir.Layout env
    ) => Glue env (Aligned a) (Aligned b) where
    type Glued (Aligned a) (Aligned b) = Aligned (Glued a b)
    glue env o a b =
        glueHelper fst env o (a ^. absAligned) (b ^. absAligned) ^.
        Lens.from absAligned

instance
    ( SizedElement a, SizedElement b
    , Glue env a b, Has Dir.Layout env
    ) => Glue env (WithTextPos a) (WithTextPos b) where
    type Glued (WithTextPos a) (WithTextPos b) = WithTextPos (Glued a b)
    -- | Vertical glue takes the top text pos
    glue env o (WithTextPos ay a) (WithTextPos by b) =
        WithTextPos y glued
        where
            (Vector2 0 y, glued) =
                glueHelper fst env o (Vector2 0 ay, a) (Vector2 0 by, b)

instance Glue env a (Widget b) => Glue env (WithTextPos a) (Widget b) where
    type Glued (WithTextPos a) (Widget b) = WithTextPos (Glued a (Widget b))
    glue env o (WithTextPos y a) b = WithTextPos y (glue env o a b)

instance
    ( SizedElement (Widget a), Glue env (Widget a) b
    ) => Glue env (Widget a) (WithTextPos b) where
    type Glued (Widget a) (WithTextPos b) = WithTextPos (Glued (Widget a) b)
    glue env o a (WithTextPos y b) =
        WithTextPos
        { _textTop =
            case o of
            Dir.Vertical -> y + a ^. Element.height
            Dir.Horizontal -> y
        , _tValue = glue env o a b
        }

instance Glue env a View => Glue env (WithTextPos a) View where
    type Glued (WithTextPos a) View = WithTextPos (Glued a View)
    glue env o (WithTextPos y a) b = WithTextPos y (glue env o a b)

instance Glue env View a => Glue env View (WithTextPos a) where
    type Glued View (WithTextPos a) = WithTextPos (Glued View a)
    glue env o a (WithTextPos y b) =
        WithTextPos
        { _textTop =
            case o of
            Dir.Vertical -> y + a ^. Element.height
            Dir.Horizontal -> y
        , _tValue = glue env o a b
        }

glueHelper ::
    (Glue env a b, Element b, SizedElement a, Has Dir.Layout env) =>
    ((Vector2 R, Vector2 R) -> Vector2 R) ->
    env -> Dir.Orientation ->
    (Vector2 R, a) -> (Vector2 R, b) -> (Vector2 R, Glued a b)
glueHelper chooseAlign env orientation (aAbsAlign, aw) (bAbsAlign, bw) =
    ( chooseAlign
        ( aAbsAlign + max 0 aToB
        , bAbsAlign + max 0 bToA + bGlueTranslation
        )
    , glue env orientation (syncAlign aToB aw) (syncAlign bToA bw)
    )
    where
        l :: Lens' (Vector2 a) a
        l = Dir.axis orientation

        -- Duplicates the logic from underlying glue:
        bGlueTranslation = 0 & l .~ aw ^. Element.size . l
        aToB = bAbsAlign - aAbsAlign & l .~ 0
        bToA = -aToB
        syncAlign :: Element a => Vector2 R -> a -> a
        syncAlign move = Element.pad env (max 0 move) 0

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

boxAlign ::
    (MonadReader env m, SizedElement a, GluesTo env a a a, Glue.HasTexts env) =>
    m (Dir.Orientation -> Widget.R -> [a] -> a)
boxAlign = Glue.box <&> \box o r xs -> box o (xs <&> Aligned (pure r)) ^. value

vboxAlign ::
    (MonadReader env m, SizedElement a, GluesTo env a a a, Glue.HasTexts env) =>
    m (Widget.R -> [a] -> a)
vboxAlign = boxAlign ?? Dir.Vertical

hboxAlign ::
    (MonadReader env m, SizedElement a, GluesTo env a a a, Glue.HasTexts env) =>
    m (Widget.R -> [a] -> a)
hboxAlign = boxAlign ?? Dir.Horizontal
