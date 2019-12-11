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
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive
    ( Responsive(..)
    , rWide, rWideDisambig, rNarrow
    , layoutWidth, vbox, fromView, vertLayoutMaybeDisambiguate
    )
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer

import           GUI.Momentu.Prelude

data WideLayouts f = WideLayouts
    { _lWide :: TextWidget f
    , _lWideDisambig :: TextWidget f
    }
Lens.makeLenses ''WideLayouts

data WideLayoutOption t f = WideLayoutOption
    { _wContexts ::
        Lens.ATraversal
        (t (Responsive f)) (t (TextWidget f))
        (Responsive f) (WideLayouts f)
    , _wLayout :: t (TextWidget f) -> WideLayouts f
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

type HorizDisambiguator f = TextWidget f -> TextWidget f

makeWideLayouts :: HorizDisambiguator a -> TextWidget a -> WideLayouts a
makeWideLayouts disamb w =
    WideLayouts
    { _lWide = w
    , _lWideDisambig = disamb w
    }

hbox ::
    (MonadReader env m, Glue.HasTexts env, Applicative f) =>
    m (HorizDisambiguator f -> ([TextWidget f] -> [TextWidget f]) -> WideLayoutOption [] f)
hbox =
    Glue.hbox <&>
    \box disamb spacer ->
    WideLayoutOption
    { _wContexts =
        -- TODO: Better way to do this?
        traverse <&> Lens.mapped . Lens.mapped . Lens.mapped %~ (^. lWideDisambig)
    , _wLayout = makeWideLayouts disamb . box . spacer
    }

table ::
    ( MonadReader env m, Traversable t0, Traversable t1, Applicative f
    , Grid.HasTexts env
    ) =>
    m (WideLayoutOption (Compose t0 t1) f)
table =
    Grid.make <&>
    \makeGrid ->
    WideLayoutOption
    { _wContexts =
        -- TODO: Better way to do this?
        traverse <&> Lens.mapped . Lens.mapped . Lens.mapped %~ (^. lWide)
    , _wLayout =
        \(Compose elems) ->
        let (alignments, gridWidget) =
                elems <&> Lens.mapped %~ toAligned & makeGrid
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
    (Applicative f, MonadReader env m, Glue.HasTexts env) =>
    m
    ( ([TextWidget f] -> [TextWidget f]) ->
      ([Responsive f] -> [Responsive f]) -> Disambiguators f ->
      [Responsive f] -> Responsive f )
boxH =
    (,) <$> hbox <*> vbox
    <&> \(horiz, vert) onHGuis onVGuis disamb guis ->
    vert (onVGuis guis)
    & vertLayoutMaybeDisambiguate (disamb ^. disambVert)
    & tryWideLayout (horiz (disamb ^. disambHoriz) onHGuis) guis

box ::
    (Applicative f, MonadReader env m, Glue.HasTexts env) =>
    m (Disambiguators f -> [Responsive f] -> Responsive f)
box = boxH ?? id ?? id

boxSpaced ::
    ( Applicative f, MonadReader env m, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (Disambiguators f -> [Responsive f] -> Responsive f)
boxSpaced =
    do
        hSpace <- Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0
        vSpace <- Spacer.stdVSpace <&> fromView
        boxH ?? List.intersperse hSpace ?? List.intersperse vSpace
