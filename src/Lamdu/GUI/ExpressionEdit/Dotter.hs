{-# LANGUAGE FlexibleContexts #-}
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
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import           Data.Has (Has)
import qualified Data.Text as Text
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as CodeUI
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

add ::
    ( MonadReader env m, Applicative o, Has TextView.Style env, Has Config env
    , Language.HasLanguage env, Element.HasAnimIdPrefix env
    ) =>
    Sugar.Payload name i o a ->
    m (Gui Responsive o -> Gui Responsive o)
add pl =
    do
        ev <- eventMap ?? pl
        label <- Label.make "?"
        (|||) <- Glue.mkGlue ?? Glue.Horizontal
        let f r =
                r ||| label
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
    ( MonadReader env m, Has Config env, Language.HasLanguage env
    , Applicative o
    ) =>
    m (Sugar.Payload name i o expr -> Gui EventMap o)
eventMap =
    (,) <$> Lens.view id <*> delDotEventMap
    <&> \(env, delDotEvents) pl ->
    delDotEvents (WidgetIds.fromExprPayload pl)
    <> fragmentEventMap env pl

-- | Each expression that may have a dotter should use this to make
-- sure it activates it when it's jumped to
with ::
    ( MonadReader env m, Applicative o, GuiState.HasCursor env
    , Has TextView.Style env, Has Config env, Element.HasAnimIdPrefix env
    , Language.HasLanguage env
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
    (Language.HasLanguage env, Applicative o) =>
    env -> Sugar.Payload name i o expr -> Gui EventMap o
fragmentEventMap env pl =
    E.charEventMap "Letter"
    (E.toDoc (env ^. Language.texts) [Texts.edit, Texts.codeUI . CodeUI.getField])
    getField
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
    ( MonadReader env m, Has Config env, Applicative f
    , Language.HasLanguage env
    ) =>
    m (Widget.Id -> Gui EventMap f)
delDotEventMap =
    (,) <$> Lens.view id <*> Config.delKeys
    <&>
    \(env, delKeys) widgetId ->
    pure widgetId
    & E.keysEventMapMovesCursor delKeys
    (E.toDoc (env ^. Language.texts)
        [Texts.edit, Texts.codeUI . CodeUI.deleteDot])

addEventMap ::
    ( Applicative f, Widget.HasWidget w, MonadReader env m
    , Language.HasLanguage env
    ) =>
    m (Widget.Id -> Gui w f -> Gui w f)
addEventMap =
    Lens.view id
    <&> \env myId ->
    let f ctx
            | Text.null (ctx ^. Widget.ePrevTextRemainder) = gotoDotter
            | otherwise = mempty
        gotoDotter =
            WidgetIds.dotterId myId & GuiState.updateCursor & pure & const
            & E.charGroup Nothing
            (E.toDoc (env ^. Language.texts)
                [Texts.edit, Texts.codeUI . CodeUI.dot]) "."
    in  Widget.weakerEventsWithContext f
