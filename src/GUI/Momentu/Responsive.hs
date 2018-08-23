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

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, FlexibleContexts #-}

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
    , TaggedItem(..), tagPre, taggedItem, tagPost
    , taggedList
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue(..), GluesTo, (/|/), (/-/), Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           Lamdu.Prelude

data NarrowLayoutParams = NarrowLayoutParams
    { _layoutWidth :: Widget.R
    , _layoutNeedDisambiguation :: Bool
    }
Lens.makeLenses ''NarrowLayoutParams

data Responsive a = Responsive
    { _rWide :: WithTextPos (Widget a)
    , _rWideDisambig :: WithTextPos (Widget a)
    , _rNarrow :: NarrowLayoutParams -> WithTextPos (Widget a)
    } deriving Functor
Lens.makeLenses ''Responsive

adjustNarrowLayoutParams :: SizedElement v => Orientation -> v -> NarrowLayoutParams -> NarrowLayoutParams
adjustNarrowLayoutParams Vertical _ = layoutNeedDisambiguation .~ True
adjustNarrowLayoutParams Horizontal v = layoutWidth -~ v ^. Element.size . _1

instance ( GluesTo (WithTextPos (Widget a)) (WithTextPos b) (WithTextPos (Widget a))
         , SizedElement b
         ) => Glue (Responsive a) (WithTextPos b) where
    type Glued (Responsive a) (WithTextPos b) = Responsive a
    glue orientation l v =
        Responsive
        { _rWide = glue orientation wide v
        , _rWideDisambig = glue orientation wide v
        , _rNarrow =
            l ^. rNarrow
            & Lens.argument %~ adjustNarrowLayoutParams orientation v
            <&> (glue orientation ?? v)
        }
        where
            wide =
                case orientation of
                Horizontal -> l ^. rWideDisambig
                Vertical -> l ^. rWide

instance ( GluesTo (WithTextPos a) (WithTextPos (Widget b)) (WithTextPos (Widget b))
         , SizedElement a
         ) => Glue (WithTextPos a) (Responsive b) where
    type Glued (WithTextPos a) (Responsive b) = Responsive b
    glue orientation v l =
        Responsive
        { _rWide = glue orientation v wide
        , _rWideDisambig = glue orientation v wide
        , _rNarrow =
            l ^. rNarrow
            & Lens.argument %~ adjustNarrowLayoutParams orientation v
            <&> glue orientation v
        }
        where
            wide =
                case orientation of
                Horizontal -> l ^. rWideDisambig
                Vertical -> l ^. rWide

instance (Functor f, a ~ f State.Update) => Element (Responsive a) where
    setLayers = Widget.widget . Element.setLayers
    hoverLayers = Widget.widget %~ Element.hoverLayers
    empty = Responsive Element.empty Element.empty (const Element.empty)
    scale = error "Responsive: scale not Implemented"
    assymetricPad topLeft bottomRight w =
        Responsive
        { _rWide = w ^. rWide & Element.assymetricPad topLeft bottomRight
        , _rWideDisambig = w ^. rWideDisambig & Element.assymetricPad topLeft bottomRight
        , _rNarrow =
            w ^. rNarrow
            & Lens.argument . layoutWidth -~ topLeft ^. _1 + bottomRight ^. _1
            <&> Element.assymetricPad topLeft bottomRight
        }

instance Widget.HasWidget Responsive where widget = alignedWidget . Align.tValue

alignedWidget ::
    Lens.Setter
    (Responsive a) (Responsive b)
    (WithTextPos (Widget a)) (WithTextPos (Widget b))
alignedWidget f (Responsive w wd n) =
    Responsive
    <$> f w
    <*> f wd
    <*> Lens.mapped f n

-- | Lifts a Widget into a 'Responsive'
fromAlignedWidget :: Functor f => Aligned (Gui Widget f) -> Gui Responsive f
fromAlignedWidget (Aligned a w) =
    WithTextPos (a ^. _2 * w ^. Element.height) w & fromWithTextPos

fromWithTextPos :: WithTextPos (Widget a) -> Responsive a
fromWithTextPos x = Responsive x x (const x)

-- | Lifts a Widget into a 'Responsive' with an alignment point at the top left
fromWidget :: Functor f => Gui Widget f -> Gui Responsive f
fromWidget = fromAlignedWidget . Aligned 0

-- | Lifts a View into a 'Responsive' with an alignment point at the top left
fromView :: Functor f => View -> Gui Responsive f
fromView = fromWidget . Widget.fromView

-- | Lifts a View into a 'Responsive' with an alignment point at the top left
fromTextView :: WithTextPos View -> Responsive a
fromTextView tv = tv & Align.tValue %~ Widget.fromView & fromWithTextPos

-- | The empty 'Responsive'
empty :: Functor f => Gui Responsive f
empty = fromView Element.empty

data VerticalLayout t a = VerticalLayout
    { _vContexts ::
        -- The width in the index is the width to remove from the child
        Lens.AnIndexedTraversal NarrowLayoutParams
        (t (Responsive a)) (t (WithTextPos (Widget a)))
        (Responsive a) (WithTextPos (Widget a))
    , _vLayout :: t (WithTextPos (Widget a)) -> WithTextPos (Widget a)
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
    Applicative f =>
    [Gui Responsive f] -> Gui Responsive f
vbox =
    verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout = Glue.vbox
    }
    where
        idx =
            NarrowLayoutParams
            { _layoutWidth = 0
            , _layoutNeedDisambiguation = True
            }

vboxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Applicative f) =>
    m ([Gui Responsive f] -> Gui Responsive f)
vboxSpaced =
    Spacer.stdVSpace
    <&> fromView
    <&> List.intersperse
    <&> Lens.mapped %~ vbox

vboxWithSeparator ::
    Applicative f =>
    Bool -> (Widget.R -> View) ->
    Gui Responsive f -> Gui Responsive f ->
    Gui Responsive f
vboxWithSeparator needDisamb makeSeparator top bottom =
    Vector2 top bottom
    & verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout =
        \(Vector2 t b) ->
        t
        /-/
        makeSeparator (max (t ^. Element.width) (b ^. Element.width))
        /-/
        b
    }
    where
        idx =
            NarrowLayoutParams
            { _layoutWidth = 0
            , _layoutNeedDisambiguation = needDisamb
            }

data TaggedItem a = TaggedItem
    { _tagPre :: WithTextPos (Widget a)
    , _taggedItem :: Responsive a
    , _tagPost :: WithTextPos (Widget a)
    } deriving Functor

Lens.makeLenses ''TaggedItem

taggedList ::
    (MonadReader env m, Spacer.HasStdSpacing env, Applicative f) =>
    m ([Gui TaggedItem f] -> Gui Responsive f)
taggedList =
    Spacer.stdVSpace <&> Widget.fromView <&> WithTextPos 0
    <&>
    \vspace items ->
    let preWidth = items ^.. traverse . tagPre . Element.width & maximum
        postWidth = items ^.. traverse . tagPost . Element.width & maximum
        renderItem ((pre, post), item) =
            ( Element.assymetricPad (Vector2 (preWidth - pre ^. Element.width) 0) 0 pre
                /|/ item
            , post
            )
        renderItems xs =
            xs <&> renderRow & List.intersperse vspace & Glue.vbox
            where
                renderRow (item, post) =
                    item /|/
                    Element.assymetricPad (Vector2 (itemWidth - item ^. Element.width) 0) 0 post
                itemWidth = xs ^.. traverse . _1 . Element.width & maximum
        idx =
            NarrowLayoutParams
            { _layoutWidth = preWidth + postWidth
            , _layoutNeedDisambiguation = False
            }
    in
    items <&> prepItem & Compose
    & verticalLayout VerticalLayout
    { _vContexts = Lens.reindexed (const idx) Lens.traversed
    , _vLayout = renderItems . map renderItem . getCompose
    }
    where
        prepItem (TaggedItem pre x post) = ((pre, post), x)

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
