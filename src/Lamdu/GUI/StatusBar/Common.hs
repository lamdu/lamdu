-- | Common utilities for status bar widgets
{-# LANGUAGE TemplateHaskell, RankNTypes, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Lamdu.GUI.StatusBar.Common
    ( StatusWidget(..), widget, globalEventMap
    , LabelConstraints
    , hoist
    , makeSwitchStatusWidget
    , fromWidget, combine, combineEdges
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import           Data.Property (Property(..))
import           GUI.Momentu.Align (WithTextPos(..), TextWidget, tValue)
import           GUI.Momentu.Animation.Id (ElemIds(..))
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element(..))
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (R)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing)
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.I18N.StatusBar as Texts

import           Lamdu.Prelude

data StatusWidget f = StatusWidget
    { _widget :: TextWidget f
    , _globalEventMap :: EventMap (f GuiState.Update)
    }
Lens.makeLenses ''StatusWidget

instance Functor f => Element (StatusWidget f) where
    setLayeredImage = widget . setLayeredImage
    hoverLayeredImage = widget %~ hoverLayeredImage
    padImpl x y = widget %~ padImpl x y
    scale x = widget %~ scale x
    empty = StatusWidget Element.empty mempty

hoist :: (f GuiState.Update -> g GuiState.Update) -> StatusWidget f -> StatusWidget g
hoist f (StatusWidget w e) =
    StatusWidget
    { _widget = w & tValue . Widget.updates %~ f
    , _globalEventMap = e <&> f
    }

fromWidget :: TextWidget f -> StatusWidget f
fromWidget w =
    StatusWidget { _widget = w, _globalEventMap = mempty }

type LabelConstraints env m =
    ( MonadReader env m, Has TextView.Style env, Has Theme env
    , Element.HasAnimIdPrefix env, Has (Texts.StatusBar Text) env
    , Has Dir.Layout env
    )

makeChoice ::
    ( MonadReader env m, Applicative f, Eq a
    , Has Hover.Style env, GuiState.HasCursor env
    , Element.HasAnimIdPrefix env
    , Has (Choice.Texts Text) env
    , Has (t Text) env, ElemIds t
    , Glue.HasTexts env
    ) =>
    OneOf t -> Property f a ->
    [(a, TextWidget f)] -> m (TextWidget f)
makeChoice headerText prop choices =
    do
        defConf <- Choice.defaultConfig
        text <- Lens.view (has . headerText)
        Choice.make ?? prop ?? choices ?? defConf text ?? myId
    where
        myId = Widget.Id ("status" : elemIds ^# headerText)

labeledChoice ::
    ( MonadReader env m, Applicative f, Eq a
    , Element.HasAnimIdPrefix env
    , GuiState.HasCursor env, Has Hover.Style env
    , Has (Choice.Texts Text) env
    , Has (t Text) env, ElemIds t
    , Glue.HasTexts env
    ) =>
    WithTextPos View -> OneOf t -> Property f a -> [(a, TextWidget f)] -> m (TextWidget f)
labeledChoice headerView categoryTextLens prop choices =
    pure headerView /|/ makeChoice categoryTextLens prop choices

makeSwitchStatusWidget ::
    ( MonadReader env m, Applicative f, Eq a
    , Has Config env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Has Hover.Style env
    , Has (Choice.Texts Text) env
    , Has (Texts.StatusBar Text) env
    , Has (t Text) env, ElemIds t
    , Glue.HasTexts env
    ) =>
    m (WithTextPos View) -> OneOf t -> OneOf Texts.StatusBar -> Lens' Config [MetaKey] -> Property f a ->
    [(a, TextWidget f)] -> m (StatusWidget f)
makeSwitchStatusWidget mkHeaderWidget categoryTextLens switchTextLens keysGetter prop choiceVals =
    do
        header <- mkHeaderWidget
        w <- labeledChoice header categoryTextLens prop choiceVals
        keys <- Lens.view (has . keysGetter)
        txt <- Lens.view has
        let e =
                setVal newVal
                & E.keysEventMap keys
                (E.toDoc txt
                    [ Texts.sbStatusBar
                    , switchTextLens
                    ])
        pure StatusWidget
            { _widget = w
            , _globalEventMap = e
            }
    where
        choices = choiceVals <&> fst
        newVal = dropWhile (/= curVal) choices ++ choices & tail & head
        Property curVal setVal = prop

hspacer ::
    (MonadReader env m, Spacer.HasStdSpacing env, Has Theme env) => m View
hspacer = do
    hSpaceCount <- Lens.view (has . Theme.statusBar . Theme.statusBarHSpaces)
    Spacer.getSpaceSize <&> (^. _1) <&> (* hSpaceCount) <&> Spacer.makeHorizontal

combine ::
    ( MonadReader env m, Applicative f, HasStdSpacing env, Has Theme env
    , Glue.HasTexts env
    ) => m ([StatusWidget f] -> StatusWidget f)
combine =
    (,,) <$> (Glue.mkPoly ?? Glue.Horizontal) <*> Glue.hbox <*> hspacer
    <&> \(Glue.Poly (|||), hbox, space) statusWidgets ->
    StatusWidget
    { _widget =
        case statusWidgets of
        [] -> Element.empty
        (x:xs) ->
            xs
            <&> (^. widget)
            <&> (space |||)
            & hbox
            & ((x ^. widget) |||)
    , _globalEventMap = statusWidgets ^. Lens.folded . globalEventMap
    }

combineEdges ::
    (MonadReader env m, Applicative f, Glue.HasTexts env) =>
    m (R -> StatusWidget f -> StatusWidget f -> StatusWidget f)
combineEdges =
    Glue.mkPoly ?? Glue.Horizontal
    <&> \(Glue.Poly (|||)) width (StatusWidget xw xe) (StatusWidget yw ye) ->
    let padding = max 0 (width - combinedWidths)
        combinedWidths = xw ^. Element.width + yw ^. Element.width
    in  StatusWidget
        { _widget = xw ||| Spacer.makeHorizontal padding ||| yw
        , _globalEventMap = xe <> ye
        }
