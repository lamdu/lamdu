-- | When typing a dot on an expression, there are 2 possible meanings
-- of that dot:
-- 1) Get record field from the expression
-- 2) Apply operator at the right parent expr (according to precedence)
--
-- Since these 2 meanings apply the dot at different AST nodes - we
-- need a temporary state until the 2 states can be disambiguated.
--
-- Note: Expressions upon which we don't expect a get-field to be
-- applied (e.g: a literal) do not have this ambiguity, because only
-- the 2nd meaning above is applicable.
--
-- In this state, the gui presents a dot besides the dotted expression.

module Lamdu.GUI.ExpressionEdit.Dotter
    ( with, addEventMap
    ) where

import           Control.Applicative (liftA2)
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

add ::
    ( MonadReader env m, TextView.HasStyle env, HasConfig env
    , Element.HasAnimIdPrefix env, Applicative o
    ) =>
    Sugar.Payload name i o a ->
    m (Gui Responsive o -> Gui Responsive o)
add pl =
    do
        ev <- eventMap pl
        label <- TextView.makeLabel "?"
        let f r =
                r /|/ label
                & Widget.setFocused
                & Widget.weakerEvents ev
                & Widget.widget %~ Widget.addPreEventWith (liftA2 mappend) preEvent
        pure f
    where
        preEvent =
            Widget.PreEvent
            { Widget._pDesc = ""
            , Widget._pAction = pure mempty
            , Widget._pTextRemainder = "."
            }

eventMap ::
    (MonadReader env m, HasConfig env, Applicative o) =>
    Sugar.Payload name i o expr -> m (Gui EventMap o)
eventMap pl =
    delDotEventMap (WidgetIds.fromExprPayload pl)
    <&> (<> fragmentEventMap pl)

-- | Each expression that may have a dotter should use this to make
-- sure it activates it when it's jumped to
with ::
    ( MonadReader env m, GuiState.HasCursor env, TextView.HasStyle env
    , HasConfig env, Element.HasAnimIdPrefix env, Applicative o
    ) =>
    Sugar.Payload name i o a ->
    m (Gui Responsive o -> Gui Responsive o)
with pl =
    do
        isActive <-
            GuiState.isSubCursor
            ?? WidgetIds.dotterId (WidgetIds.fromExprPayload pl)
        if isActive
            then add pl
            else pure id

-- | Pressing alpha char transforms the dotted expr into a fragment
-- with '.<char>' as the search term
fragmentEventMap ::
    Applicative o => Sugar.Payload name i o expr -> Gui EventMap o
fragmentEventMap pl =
    E.charEventMap "Character" (E.Doc ["Edit", "Get field"]) getField
    where
        detach (Sugar.FragmentAlready entityId) = pure entityId
        detach (Sugar.FragmentExprAlready entityId) = pure entityId
        detach (Sugar.DetachAction act) = act
        getField c
            | Char.isAlpha c =
                pl ^. Sugar.plActions . Sugar.detach & detach
                <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
                <&> SearchMenu.enterWithSearchTerm ("." <> Text.singleton c)
                & Just
            | otherwise = Nothing

delDotEventMap ::
    (MonadReader env m, HasConfig env, Applicative f) =>
    Widget.Id -> m (Gui EventMap f)
delDotEventMap widgetId =
    Config.delKeys
    <&>
    \delKeys ->
    pure widgetId
    & E.keysEventMapMovesCursor delKeys (E.Doc ["Edit", "Delete dot"])

addEventMap ::
    (Applicative f, Widget.HasWidget w) => Widget.Id -> Gui w f -> Gui w f
addEventMap myId =
    Widget.weakerEventsWithContext f
    where
        f ctx
            | Text.null (ctx ^. Widget.ePrevTextRemainder) = gotoDotter
            | otherwise = mempty
        gotoDotter =
            WidgetIds.dotterId myId & GuiState.updateCursor & pure & const
            & E.charGroup Nothing (E.Doc ["Edit", "Dot"]) "."
