{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module GUI.Momentu.Responsive.Options
    ( WideLayoutOption(..), wContexts, wLayout
    , tryWideLayout
    , hbox, table

    , Disambiguators(..), disambHoriz, disambVert
    , disambiguationNone
    , box, boxSpaced
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Compose (Compose(..))
import qualified Data.List as List
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive
    ( Responsive(..), LayoutParams(..), LayoutMode(..), LayoutDisambiguationContext(..)
    , render, vbox, fromView
    )
import           GUI.Momentu.Widget (Widget, EventResult)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           Lamdu.Prelude

data WideLayoutOption t a = WideLayoutOption
    { _wContexts ::
        Lens.AnIndexedTraversal LayoutDisambiguationContext
        (t (Responsive a)) (t (WithTextPos (Widget a)))
        (Responsive a) (WithTextPos (Widget a))
    , _wLayout ::
        LayoutDisambiguationContext ->
        t (WithTextPos (Widget a)) ->
        WithTextPos (Widget a)
    }
Lens.makeLenses ''WideLayoutOption

tryWideLayout :: WideLayoutOption t a -> t (Responsive a) -> Responsive a -> Responsive a
tryWideLayout layoutOption elements fallback =
    Responsive $
    \layoutParams ->
    case layoutParams of
    LayoutParams LayoutWide context -> (layoutOption ^. wLayout) context renderedElements
    LayoutParams (LayoutNarrow limit) context
        | wide ^. Align.tValue . Widget.wSize . _1 <= limit -> wide
        | otherwise -> (fallback ^. render) layoutParams
        where
            wide = (layoutOption ^. wLayout) context renderedElements
    where
        renderedElements = elements & Lens.cloneIndexedTraversal (layoutOption ^. wContexts) %@~ renderElement
        renderElement context element =
            (element ^. render)
            LayoutParams
            { _layoutMode = LayoutWide
            , _layoutContext = context
            }

hbox ::
    Functor f =>
    HorizDisambiguator (f EventResult) ->
    ([WithTextPos (Widget (f EventResult))] -> [WithTextPos (Widget (f EventResult))]) ->
    WideLayoutOption [] (f EventResult)
hbox disamb spacer =
    WideLayoutOption
    { _wContexts = Lens.reindexed (const LayoutHorizontal) Lens.traversed
    , _wLayout = layout
    }
    where
        layout c = mDisamb c . Glue.hbox . spacer
        mDisamb LayoutHorizontal = disamb
        mDisamb _ = id

table ::
    (Traversable t0, Traversable t1, Functor f) =>
    WideLayoutOption (Compose t0 t1) (f EventResult)
table =
    WideLayoutOption
    { _wContexts = Lens.reindexed (const LayoutClear) (Lens._Wrapped . Lens.traversed . Lens.traversed)
    , _wLayout = const layout
    }
    where
        layout (Compose elems) =
            WithTextPos
            { _textTop =
                gridWidget ^. Element.height
                * alignments ^?! traverse . traverse . Align.alignmentRatio . _2
            , _tValue = gridWidget
            }
            where
                (alignments, gridWidget) = elems <&> Lens.mapped %~ toAligned & Grid.make
        toAligned (WithTextPos y w) = Aligned (Vector2 0 (y / w ^. Element.height)) w

type HorizDisambiguator a = WithTextPos (Widget a) -> WithTextPos (Widget a)

data Disambiguators a = Disambiguators
    { _disambHoriz :: HorizDisambiguator a
    , _disambVert :: Responsive a -> Responsive a
    }

Lens.makeLenses ''Disambiguators

disambiguationNone :: Disambiguators a
disambiguationNone = Disambiguators id id

boxH ::
    Functor f =>
    ([WithTextPos (Widget (f EventResult))] -> [WithTextPos (Widget (f EventResult))]) ->
    ([Responsive (f EventResult)] -> [Responsive (f EventResult)]) ->
    Disambiguators (f EventResult) ->
    [Responsive (f EventResult)] ->
    Responsive (f EventResult)
boxH onHGuis onVGuis disamb guis =
    vbox (onVGuis guis)
    & tryWideLayout (hbox (disamb ^. disambHoriz) onHGuis) guis

box ::
    Functor f =>
    Disambiguators (f EventResult) ->
    [Responsive (f EventResult)] ->
    Responsive (f EventResult)
box = boxH id id

boxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Functor f) =>
    m (Disambiguators (f Widget.EventResult) -> [Responsive (f Widget.EventResult)] -> Responsive (f Widget.EventResult))
boxSpaced =
    do
        hSpace <- Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0
        vSpace <- Spacer.stdVSpace <&> fromView
        boxH (List.intersperse hSpace) (List.intersperse vSpace) & return
