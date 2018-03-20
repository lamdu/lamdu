-- | GUI choice of themes
{-# LANGUAGE OverloadedStrings #-}

module Lamdu.GUI.Themes
    ( switchEventMap
    , widgetForStatusBar
    , Themes.Selection
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

switchEventMap ::
    (MonadReader env m, Config.HasConfig env) =>
    [Text] -> Property IO Themes.Selection ->
    m (EventMap (IO GuiState.Update))
switchEventMap themeNames (Property curTheme setTheme) =
    Lens.view (Config.config . Config.changeThemeKeys)
    <&> \keys ->
    let newTheme = dropWhile (/= curTheme) themeNames ++ themeNames & tail & head
    in  setTheme newTheme
        & E.keysEventMap keys (E.Doc ["Theme", "Switch"])

widgetForStatusBar ::
    ( MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env
    , GuiState.HasCursor env, Hover.HasStyle env, Theme.HasTheme env
    , Applicative f
    ) =>
    [Themes.Selection] ->
    Property f Themes.Selection ->
    m (WithTextPos (Widget (f GuiState.Update)))
widgetForStatusBar themeNames prop =
    do
        header <-
            TextView.makeLabel "Theme "
            & Styled.withColor Theme.infoTextColor
        choices <- themeNames & traverse mkChoice
        choice <-
            Choice.make
            ?? Property.set prop ?? choices ?? cur
            ?? Choice.defaultConfig "Theme" ?? myId
            <&> WithTextPos 0 -- TODO: Choice should maintain the WithTextPos
        header /|/ choice & pure
    where
        myId = Widget.Id ["Theme Choice"]
        mkChoice theme =
            TextView.makeFocusableLabel theme
            <&> (^. Align.tValue)
            <&> (,) theme
        cur = Property.value prop
