{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module GUI.Momentu.Responsive.Options
    ( WideLayoutOption(..), wContexts, wLayout
    , tryWideLayout
    , hbox

    , Disambiguators(..), disambHoriz, disambVert
    , disambiguationNone
    , box, boxSpaced
    ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive
    ( Responsive(..), LayoutParams(..), LayoutMode(..), LayoutDisambiguationContext(..)
    , render, vbox, fromView
    )
import           GUI.Momentu.Widget (Widget, EventResult)
import qualified GUI.Momentu.Widget as Widget
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
    { _wContexts = contexts
    , _wLayout = layout
    }
    where
        contexts :: Lens.AnIndexedTraversal LayoutDisambiguationContext [a] [b] a b
        contexts _ [] = pure []
        contexts f (x:xs) =
            (:)
            <$> Lens.indexed f LayoutHorizontal x
            <*> contexts f xs
        layout c = mDisamb c . Glue.hbox . spacer
        mDisamb LayoutHorizontal = disamb
        mDisamb _ = id

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
