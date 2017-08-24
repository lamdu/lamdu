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

{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module GUI.Momentu.Responsive
    ( Responsive(..), render

    -- * Layout params
    , LayoutParams(..), layoutMode, layoutContext
    , LayoutMode(..), _LayoutNarrow, _LayoutWide
    , LayoutDisambiguationContext(..)

    -- * Lenses
    , alignedWidget, modeWidths

    -- * Leaf generation
    , fromAlignedWidget, fromWithTextPos, fromWidget, fromView, fromTextView, empty

    -- * Combinators
    , vbox, vboxSpaced, taggedList
    , vertLayoutMaybeDisambiguate
    ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue (Glue(..), GluesTo, (/|/), Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           Lamdu.Prelude

data LayoutMode
    = LayoutNarrow Widget.R -- ^ limited by the contained width field
    | LayoutWide -- ^ no limit on width
Lens.makePrisms ''LayoutMode

modeWidths :: Lens.Traversal' LayoutMode Widget.R
modeWidths _ LayoutWide = pure LayoutWide
modeWidths f (LayoutNarrow limit) = f limit <&> LayoutNarrow

-- The relevant context for knowing whether parenthesis/indentation is needed
data LayoutDisambiguationContext
    = LayoutClear
    | LayoutHorizontal
    | LayoutVertical

data LayoutParams = LayoutParams
    { _layoutMode :: LayoutMode
    , _layoutContext :: LayoutDisambiguationContext
    }
Lens.makeLenses ''LayoutParams

newtype Responsive a = Responsive
    { _render :: LayoutParams -> WithTextPos (Widget a)
    } deriving Functor
Lens.makeLenses ''Responsive

adjustWidth :: SizedElement v => Orientation -> v -> Responsive a -> Responsive a
adjustWidth Vertical _ = id
adjustWidth Horizontal v =
    render . Lens.argument . layoutMode . modeWidths -~ v ^. Element.size . _1

instance ( GluesTo (WithTextPos (Widget a)) (WithTextPos b) (WithTextPos (Widget a))
         , SizedElement b
         ) => Glue (Responsive a) (WithTextPos b) where
    type Glued (Responsive a) (WithTextPos b) = Responsive a
    glue orientation l v =
        l
        & adjustWidth orientation v
        & render . Lens.mapped %~ (glue orientation ?? v)

instance ( GluesTo (WithTextPos a) (WithTextPos (Widget b)) (WithTextPos (Widget b))
         , SizedElement a
         ) => Glue (WithTextPos a) (Responsive b) where
    type Glued (WithTextPos a) (Responsive b) = Responsive b
    glue orientation v l =
        l
        & adjustWidth orientation v
        & render . Lens.mapped %~ glue orientation v

instance Functor f => Element (Responsive (f Widget.EventResult)) where
    setLayers = Widget.widget . Element.setLayers
    hoverLayers = Widget.widget %~ Element.hoverLayers
    empty = Responsive (const Element.empty)
    pad p w =
        w
        & render . Lens.argument . layoutMode . modeWidths -~ 2 * (p ^. _1)
        & render . Lens.mapped %~ Element.pad p
    scale = error "Responsive: scale not Implemented"
    assymetricPad = error "Responsive: assymetricPad not implemented"

instance E.HasEventMap Responsive where eventMap = Widget.widget . E.eventMap

instance Widget.HasWidget Responsive where widget = alignedWidget . Align.tValue

alignedWidget ::
    Lens.Setter
    (Responsive a) (Responsive b)
    (WithTextPos (Widget a)) (WithTextPos (Widget b))
alignedWidget = render . Lens.mapped

-- | Lifts a Widget into a 'Responsive'
fromAlignedWidget ::
    Functor f =>
    Aligned (Widget (f Widget.EventResult)) -> Responsive (f Widget.EventResult)
fromAlignedWidget (Aligned a w) =
    WithTextPos (a ^. _2 * w ^. Element.height) w
    & const
    & Responsive

fromWithTextPos :: WithTextPos (Widget a) -> Responsive a
fromWithTextPos = Responsive . const

-- | Lifts a Widget into a 'Responsive' with an alignment point at the top left
fromWidget :: Functor f => Widget (f Widget.EventResult) -> Responsive (f Widget.EventResult)
fromWidget = fromAlignedWidget . Aligned 0

-- | Lifts a View into a 'Responsive' with an alignment point at the top left
fromView :: Functor f => View -> Responsive (f Widget.EventResult)
fromView = fromWidget . Widget.fromView

-- | Lifts a View into a 'Responsive' with an alignment point at the top left
fromTextView :: WithTextPos View -> Responsive a
fromTextView tv = tv & Align.tValue %~ Widget.fromView & fromWithTextPos

-- | The empty 'Responsive'
empty :: Functor f => Responsive (f Widget.EventResult)
empty = fromView Element.empty

-- | Vertical box with the alignment point from the top widget
vbox ::
    Functor f =>
    [Responsive (f Widget.EventResult)] -> Responsive (f Widget.EventResult)
vbox [] = empty
vbox (gui:guis) =
    Responsive $
    \layoutParams ->
    let cp =
            LayoutParams
            { _layoutMode = layoutParams ^. layoutMode
            , _layoutContext = LayoutVertical
            }
    in
    (gui ^. render) cp : (guis ^.. traverse . render ?? cp)
    & Glue.vbox

vboxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Functor f) =>
    m ([Responsive (f Widget.EventResult)] -> Responsive (f Widget.EventResult))
vboxSpaced =
    Spacer.stdVSpace
    <&> fromView
    <&> List.intersperse
    <&> Lens.mapped %~ vbox

taggedList ::
    (MonadReader env m, Spacer.HasStdSpacing env, Functor f) =>
    m ([(WithTextPos (Widget (f Widget.EventResult)), Responsive (f Widget.EventResult))] -> Responsive (f Widget.EventResult))
taggedList =
    vboxSpaced <&>
    \box pairs ->
    let headerWidth = pairs ^.. traverse . _1 . Element.width & maximum
        renderPair (header, treeLayout) =
            Element.assymetricPad (Vector2 (headerWidth - header ^. Element.width) 0) 0 header
            /|/ treeLayout
    in
    pairs <&> renderPair & box

-- | Apply a given vertical disambiguator (such as indentation) when necessary,
-- according to the layout context.
-- For example a vertical list inside a vertical list will require disambiguation
-- to know the inner list apart for the outer one.
vertLayoutMaybeDisambiguate ::
    (Responsive a -> Responsive a) -> Responsive a -> Responsive a
vertLayoutMaybeDisambiguate disamb vert =
    Responsive $
    \layoutParams ->
    case layoutParams ^. layoutContext of
    LayoutVertical -> (disamb vert ^. render) layoutParams
    _ -> (vert ^. render) layoutParams
