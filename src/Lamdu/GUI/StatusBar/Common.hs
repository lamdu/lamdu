-- | Common utilities for status bar widgets
{-# LANGUAGE TemplateHaskell, RankNTypes, TypeFamilies, FlexibleContexts #-}
module Lamdu.GUI.StatusBar.Common
    ( StatusWidget(..), widget, globalEventMap
    , hoist
    , makeSwitchStatusWidget, makeBoundedSwitchStatusWidget
    , makeStatusWidget
    , combine, combineEdges
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Element (Element(..))
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue (GluesTo, (/|/), hbox)
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget, R)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing)
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config, HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Styled as Styled

import           Lamdu.Prelude

data StatusWidget f = StatusWidget
    { _widget :: WithTextPos (Widget (f GuiState.Update))
    , _globalEventMap :: EventMap (f GuiState.Update)
    }
Lens.makeLenses ''StatusWidget

instance Functor f => Element (StatusWidget f) where
    setLayers = widget . setLayers
    hoverLayers = widget %~ hoverLayers
    assymetricPad x y = widget %~ assymetricPad x y
    scale x = widget %~ scale x
    empty = StatusWidget Element.empty mempty

hoist :: (f GuiState.Update -> g GuiState.Update) -> StatusWidget f -> StatusWidget g
hoist f (StatusWidget w e) =
    StatusWidget
    { _widget = w <&> fmap f
    , _globalEventMap = e <&> f
    }

makeLabeledWidget ::
    ( MonadReader env m
    , GluesTo (WithTextPos View) a b
    , HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> a -> m b
makeLabeledWidget headerText w =
    TextView.makeLabel (headerText <> " ")
    & Styled.withColor TextColors.infoTextColor
    <&> (/|/ w)

makeStatusWidget ::
    ( MonadReader env m, Functor f
    , HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => Text -> WithTextPos (Widget (f GuiState.Update)) -> m (StatusWidget f)
makeStatusWidget headerText w =
    makeLabeledWidget headerText w
    <&> (`StatusWidget` mempty)

makeChoice ::
    ( MonadReader env m, Applicative f, Eq a
    , Hover.HasStyle env, GuiState.HasCursor env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    Text -> Property f a -> [(Text, a)] ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeChoice headerText prop choiceVals =
    do
        choices <- traverse mkChoice choiceVals
        Choice.make ?? prop ?? choices ?? Choice.defaultConfig headerText ?? myId
            <&> WithTextPos 0 -- TODO: Choice should maintain the WithTextPos
    where
        myId = Widget.Id [encodeUtf8 headerText]
        mkChoice (text, val) =
            TextView.makeFocusableLabel text
            <&> (^. Align.tValue)
            <&> (,) val

makeSwitchWidget ::
    ( MonadReader env m, Applicative f, Eq a
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env
    , GuiState.HasCursor env, Hover.HasStyle env
    ) =>
    Text -> Property f a -> [(Text, a)] ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeSwitchWidget headerText prop choiceVals =
    do
        choice <- makeChoice headerText prop choiceVals
        makeLabeledWidget headerText choice

makeSwitchEventMap ::
    (MonadReader env m, HasConfig env, Eq a, Functor f) =>
    Text -> Lens' Config [MetaKey] ->
    Property f a -> [a] ->
    m (EventMap (f GuiState.Update))
makeSwitchEventMap headerText keysGetter (Property curVal setVal) choiceVals =
    Lens.view (Config.config . keysGetter)
    <&> \keys ->
    let newVal = dropWhile (/= curVal) choiceVals ++ choiceVals & tail & head
    in  setVal newVal
        & E.keysEventMap keys (E.Doc ["Status bar", "Switch " <> headerText])

makeSwitchStatusWidget ::
    ( MonadReader env m, Applicative f, Eq a
    , HasConfig env, HasTheme env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Hover.HasStyle env
    ) =>
    Text -> Lens' Config [MetaKey] ->
    Property f a -> [(Text, a)] ->
    m (StatusWidget f)
makeSwitchStatusWidget headerText keysGetter prop choiceVals =
    do
        w <- makeSwitchWidget headerText prop choiceVals
        e <- makeSwitchEventMap headerText keysGetter prop (map snd choiceVals)
        pure StatusWidget
            { _widget = w
            , _globalEventMap = e
            }

makeBoundedSwitchStatusWidget ::
    ( MonadReader env m, Applicative f, Eq a, Enum a, Bounded a, Show a
    , HasConfig env, HasTheme env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Hover.HasStyle env
    ) =>
    Text -> Lens' Config [MetaKey] ->
    Property f a ->
    m (StatusWidget f)
makeBoundedSwitchStatusWidget headerText keysGetter prop =
    makeSwitchStatusWidget headerText keysGetter prop choiceVals
    where
        choiceVals = [minBound..maxBound] <&> \val -> (Text.pack (show val), val)

hspacer ::
    (MonadReader env m, Spacer.HasStdSpacing env, Theme.HasTheme env) => m View
hspacer = do
    hSpaceCount <- Lens.view (Theme.theme . Theme.statusBar . Theme.statusBarHSpaces)
    Spacer.getSpaceSize <&> (^. _1) <&> (* hSpaceCount) <&> Spacer.makeHorizontal

combine ::
    ( MonadReader env m, Functor f
    , HasStdSpacing env, HasTheme env
    ) => m ([StatusWidget f] -> StatusWidget f)
combine =
    hspacer
    <&> \space statusWidgets ->
    StatusWidget
    { _widget =
        case statusWidgets of
        [] -> Element.empty
        (x:xs) ->
            xs
            <&> (^. widget)
            <&> (space /|/)
            & hbox
            & ((x ^. widget) /|/)
    , _globalEventMap = statusWidgets ^. Lens.folded . globalEventMap
    }

combineEdges ::
    Functor f =>
    R -> StatusWidget f -> StatusWidget f -> StatusWidget f
combineEdges width (StatusWidget xw xe) (StatusWidget yw ye) =
    StatusWidget
    { _widget = xw /|/ Spacer.makeHorizontal padding /|/ yw
    , _globalEventMap = xe <> ye
    }
    where
        padding = max 0 (width - combinedWidths)
        combinedWidths = xw ^. Element.width + yw ^. Element.width
