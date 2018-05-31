{-# LANGUAGE TemplateHaskell #-}

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
    ( Responsive(..)
    , rWide, rWideDisambig, rNarrow
    , layoutWidth , vbox, fromView, vertLayoutMaybeDisambiguate
    )
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           Lamdu.Prelude

data WideLayouts a = WideLayouts
    { _lWide :: WithTextPos (Widget a)
    , _lWideDisambig :: WithTextPos (Widget a)
    } deriving Functor
Lens.makeLenses ''WideLayouts

data WideLayoutOption t a = WideLayoutOption
    { _wContexts ::
        Lens.ATraversal
        (t (Responsive a)) (t (WithTextPos (Widget a)))
        (Responsive a) (WideLayouts a)
    , _wLayout :: t (WithTextPos (Widget a)) -> WideLayouts a
    }
Lens.makeLenses ''WideLayoutOption

tryWideLayout :: WideLayoutOption t a -> t (Responsive a) -> Responsive a -> Responsive a
tryWideLayout layoutOption elements fallback =
    Responsive
    { _rWide = wide
    , _rWideDisambig = res ^. lWideDisambig
    , _rNarrow =
        \layoutParams ->
        if wide ^. Align.tValue . Widget.wSize . _1 <= layoutParams ^. layoutWidth
        then wide
        else (fallback ^. rNarrow) layoutParams
    }
    where
        wide = res ^. lWide
        res = (layoutOption ^. wLayout) renderedElements
        renderedElements =
            elements & Lens.cloneTraversal (layoutOption ^. wContexts) %~ takeWide
        takeWide element =
            WideLayouts
            { _lWide = element ^. rWide
            , _lWideDisambig = element ^. rWideDisambig
            }

type HorizDisambiguator a = WithTextPos (Widget a) -> WithTextPos (Widget a)

makeWideLayouts :: HorizDisambiguator a -> WithTextPos (Widget a) -> WideLayouts a
makeWideLayouts disamb w =
    WideLayouts
    { _lWide = w
    , _lWideDisambig = disamb w
    }

hbox ::
    Functor f =>
    HorizDisambiguator (f State.Update) ->
    ([WithTextPos (Widget (f State.Update))] -> [WithTextPos (Widget (f State.Update))]) ->
    WideLayoutOption [] (f State.Update)
hbox disamb spacer =
    WideLayoutOption
    { _wContexts =
        -- TODO: Better way to do this?
        traverse <&> Lens.mapped . Lens.mapped . Lens.mapped %~ (^. lWideDisambig)
    , _wLayout = makeWideLayouts disamb . Glue.hbox . spacer
    }

table ::
    (Traversable t0, Traversable t1, Functor f) =>
    WideLayoutOption (Compose t0 t1) (f State.Update)
table =
    WideLayoutOption
    { _wContexts =
        -- TODO: Better way to do this?
        traverse <&> Lens.mapped . Lens.mapped . Lens.mapped %~ (^. lWide)
    , _wLayout =
        \(Compose elems) ->
        let (alignments, gridWidget) = elems <&> Lens.mapped %~ toAligned & Grid.make
        in
        makeWideLayouts id
        WithTextPos
        { _textTop =
            gridWidget ^. Element.height
            * alignments ^?! traverse . traverse . Align.alignmentRatio . _2
        , _tValue = gridWidget
        }
    }
    where
        toAligned (WithTextPos y w) = Aligned (Vector2 0 (y / w ^. Element.height)) w

data Disambiguators a = Disambiguators
    { _disambHoriz :: HorizDisambiguator a
    , _disambVert :: Responsive a -> Responsive a
    }

Lens.makeLenses ''Disambiguators

disambiguationNone :: Disambiguators a
disambiguationNone = Disambiguators id id

boxH ::
    Functor f =>
    ([WithTextPos (Widget (f State.Update))] -> [WithTextPos (Widget (f State.Update))]) ->
    ([Responsive (f State.Update)] -> [Responsive (f State.Update)]) ->
    Disambiguators (f State.Update) ->
    [Responsive (f State.Update)] ->
    Responsive (f State.Update)
boxH onHGuis onVGuis disamb guis =
    vbox (onVGuis guis)
    & vertLayoutMaybeDisambiguate (disamb ^. disambVert)
    & tryWideLayout (hbox (disamb ^. disambHoriz) onHGuis) guis

box ::
    Functor f =>
    Disambiguators (f State.Update) ->
    [Responsive (f State.Update)] ->
    Responsive (f State.Update)
box = boxH id id

boxSpaced ::
    (MonadReader env m, Spacer.HasStdSpacing env, Functor f) =>
    m (Disambiguators (f State.Update) -> [Responsive (f State.Update)] -> Responsive (f State.Update))
boxSpaced =
    do
        hSpace <- Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0
        vSpace <- Spacer.stdVSpace <&> fromView
        boxH (List.intersperse hSpace) (List.intersperse vSpace) & pure
