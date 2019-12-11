-- | Responsive is a layout form intended for visualizing tree-data,
-- such as program code.
--
-- Its design goals are:
--
-- * Make good use of the available screen real-estate.
-- * Avoid horizontal scroll
-- * Display the hierarchy/tree structure clearly
-- * Make the layout changes due to edits predictable and easy to follow
--
-- Subtrees are laid out horizontally as long as they fit within the
-- available horizontal space, to avoid horizontal scrolling.
--
-- When there is not enough horizontal space to lay the entire tree
-- horizontally, vertical layouts are used for the upper parts of the tree.
--
-- Hierarchy disambiguation happens using parentheses and indentation,
-- but only when necessary. For example: a horizontally laid out child
-- of a vertically laid out parent will not use parentheses as the
-- hierarchy is already clear in the layout itself.

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module GUI.Momentu.Responsive
    ( Responsive(..), rWide, rWideDisambig, rNarrow

    -- * Layout params
    , NarrowLayoutParams(..), layoutWidth, layoutNeedDisambiguation

    -- * Lenses
    , alignedWidget

    -- * Leaf generation
    , fromAlignedWidget, fromWithTextPos, fromWidget, fromView, fromTextView, empty

    -- * Combinators
    , vbox, vboxSpaced, vboxWithSeparator
    , vertLayoutMaybeDisambiguate

    , VerticalLayout(..), vContexts, vLayout
    , verticalLayout
    ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Direction (Orientation(..))
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue(..), GluesTo)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           GUI.Momentu.Prelude

data NarrowLayoutParams = NarrowLayoutParams
    { _layoutWidth :: Widget.R
    , _layoutNeedDisambiguation :: Bool
    }
Lens.makeLenses ''NarrowLayoutParams

data Responsive f = Responsive
    { _rWide :: TextWidget f
    , _rWideDisambig :: TextWidget f
    , _rNarrow :: NarrowLayoutParams -> TextWidget f
    }
Lens.makeLenses ''Responsive

adjustNarrowLayoutParams ::
    SizedElement v =>
    Orientation -> v -> NarrowLayoutParams -> NarrowLayoutParams
adjustNarrowLayoutParams Vertical _ = layoutNeedDisambiguation .~ True
adjustNarrowLayoutParams Horizontal v = layoutWidth -~ v ^. Element.size . _1

instance
    ( GluesTo env (TextWidget a) (WithTextPos b) (TextWidget a)
    , SizedElement b
    ) => Glue env (Responsive a) (WithTextPos b) where
    type Glued (Responsive a) (WithTextPos b) = Responsive a
    glue env orientation l v =
        Responsive
        { _rWide = glue env orientation wide v
        , _rWideDisambig = glue env orientation wide v
        , _rNarrow =
            l ^. rNarrow
            & Lens.argument %~ adjustNarrowLayoutParams orientation v
            <&> (glue env orientation ?? v)
        }
        where
            wide =
                case orientation of
                Horizontal -> l ^. rWideDisambig
                Vertical -> l ^. rWide

instance
    ( GluesTo env (WithTextPos a) (TextWidget b) (TextWidget b)
    , SizedElement a
    ) => Glue env (WithTextPos a) (Responsive b) where
    type Glued (WithTextPos a) (Responsive b) = Responsive b
    glue env orientation v l =
        Responsive
        { _rWide = glue env orientation v wide
        , _rWideDisambig = glue env orientation v wide
        , _rNarrow =
            l ^. rNarrow
            & Lens.argument %~ adjustNarrowLayoutParams orientation v
            <&> glue env orientation v
        }
        where
            wide =
                case orientation of
                Horizontal -> l ^. rWideDisambig
                Vertical -> l ^. rWide

instance Functor f => Element (Responsive f) where
    setLayeredImage = Widget.widget . Element.setLayeredImage
    hoverLayeredImage = Widget.widget %~ Element.hoverLayeredImage
    empty = Responsive Element.empty Element.empty (const Element.empty)
    scale = error "Responsive: scale not Implemented"
    padImpl topLeft bottomRight w =
        Responsive
        { _rWide = w ^. rWide & Element.padImpl topLeft bottomRight
        , _rWideDisambig = w ^. rWideDisambig & Element.padImpl topLeft bottomRight
        , _rNarrow =
            w ^. rNarrow
            & Lens.argument . layoutWidth -~ topLeft ^. _1 + bottomRight ^. _1
            <&> Element.padImpl topLeft bottomRight
        }

instance Widget.HasWidget Responsive where widget = alignedWidget . Align.tValue

alignedWidget ::
    Lens.Setter
    (Responsive a) (Responsive b)
    (TextWidget a) (TextWidget b)
alignedWidget f (Responsive w wd n) =
    Responsive
    <$> f w
    <*> f wd
    <*> Lens.mapped f n

-- | Lifts a Widget into a 'Responsive'
fromAlignedWidget :: Functor f => Aligned (Widget f) -> Responsive f
fromAlignedWidget (Aligned a w) =
    WithTextPos (a ^. _2 * w ^. Element.height) w & fromWithTextPos

fromWithTextPos :: TextWidget a -> Responsive a
fromWithTextPos x = Responsive x x (const x)

-- | Lifts a Widget into a 'Responsive' with an alignment point at the top left
fromWidget :: Functor f => Widget f -> Responsive f
fromWidget = fromAlignedWidget . Aligned 0

-- | Lifts a View into a 'Responsive' with an alignment point at the top left
fromView :: Functor f => View -> Responsive f
fromView = fromWidget . Widget.fromView

-- | Lifts a View into a 'Responsive' with an alignment point at the top left
fromTextView :: WithTextPos View -> Responsive a
fromTextView tv = tv & Align.tValue %~ Widget.fromView & fromWithTextPos

-- | The empty 'Responsive'
empty :: Functor f => Responsive f
empty = fromView Element.empty

data VerticalLayout t a = VerticalLayout
    { _vContexts ::
        -- The width in the index is the width to remove from the child
        Lens.AnIndexedTraversal NarrowLayoutParams
        (t (Responsive a)) (t (TextWidget a))
        (Responsive a) (TextWidget a)
    , _vLayout :: t (TextWidget a) -> TextWidget a
    }
Lens.makeLenses ''VerticalLayout

verticalLayout :: Functor t => VerticalLayout t a -> t (Responsive a) -> Responsive a
verticalLayout vert items =
    Responsive
    { _rWide = wide
    , _rWideDisambig = wide
    , _rNarrow =
        \layoutParams ->
        let onItem params item =
                (item ^. rNarrow)
                NarrowLayoutParams
                { _layoutNeedDisambiguation = params ^. layoutNeedDisambiguation
                , _layoutWidth = layoutParams ^. layoutWidth - params ^. layoutWidth
                }
        in
        (vert ^. vLayout) (items & Lens.cloneIndexedTraversal (vert ^. vContexts) %@~ onItem)
    }
    where
        wide = (vert ^. vLayout) (items <&> (^. rWide))

-- | Vertical box with the alignment point from the top widget
vbox ::
    (MonadReader env m, Applicative f, Glue.HasTexts env) =>
    m ([Responsive f] -> Responsive f)
vbox =
    Glue.vbox <&> \vert ->
    verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout = vert
    }
    where
        idx =
            NarrowLayoutParams
            { _layoutWidth = 0
            , _layoutNeedDisambiguation = True
            }

vboxSpaced ::
    ( MonadReader env m, Spacer.HasStdSpacing env, Glue.HasTexts env
    , Applicative f
    ) =>
    m ([Responsive f] -> Responsive f)
vboxSpaced =
    (,) <$> vbox <*> Spacer.stdVSpace
    <&>
    (\(vert, space) -> List.intersperse (fromView space) <&> vert)

vboxWithSeparator ::
    (MonadReader env m, Applicative f, Glue.HasTexts env) =>
    m
    (Bool -> (Widget.R -> View) ->
     Responsive f -> Responsive f ->
     Responsive f)
vboxWithSeparator =
    Glue.mkPoly ?? Vertical
    <&> \(Glue.Poly (|---|)) needDisamb makeSeparator top bottom ->
    let idx =
            NarrowLayoutParams
            { _layoutWidth = 0
            , _layoutNeedDisambiguation = needDisamb
            }
    in  Vector2 top bottom
        & verticalLayout VerticalLayout
        { _vContexts = Lens.reindexed (const idx) Lens.traversed
        , _vLayout =
            \(Vector2 t b) ->
            t
            |---|
            makeSeparator (max (t ^. Element.width) (b ^. Element.width))
            |---|
            b
        }

-- | Apply a given vertical disambiguator (such as indentation) when necessary,
-- according to the layout context.
-- For example a vertical list inside a vertical list will require disambiguation
-- to know the inner list apart for the outer one.
vertLayoutMaybeDisambiguate ::
    (Responsive a -> Responsive a) -> Responsive a -> Responsive a
vertLayoutMaybeDisambiguate disamb vert =
    vert & rNarrow .~
    \layoutParams ->
    if layoutParams ^. layoutNeedDisambiguation
    then (disamb vert ^. rNarrow) layoutParams
    else (vert ^. rNarrow) layoutParams
