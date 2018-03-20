-- | The widget/event map that controls the annotation mode
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.AnnotationMode.Widget
    ( switchEventMap
    , forStatusBar
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Property as Property
import qualified Data.Text as Text
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
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode(..))
import qualified Lamdu.GUI.CodeEdit.AnnotationMode as AnnotationMode
import qualified Lamdu.GUI.Styled as Styled

import           Lamdu.Prelude

forStatusBar ::
    ( MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env
    , GuiState.HasCursor env, Hover.HasStyle env, Theme.HasTheme env
    , Applicative f
    ) =>
    Property f AnnotationMode ->
    m (WithTextPos (Widget (f GuiState.Update)))
forStatusBar prop =
    do
        header <-
            TextView.makeLabel "Annotations "
            & Styled.withColor TextColors.infoTextColor
        choices <- [minBound..maxBound] & traverse mkChoice
        choice <-
            Choice.make
            ?? Property.set prop ?? choices ?? cur
            ?? Choice.defaultConfig "Annotation Mode" ?? myId
            <&> WithTextPos 0 -- TODO: Choice should maintain the WithTextPos
        header /|/ choice & pure
    where
        myId = Widget.Id ["Annotations Mode Choice"]
        mkChoice aMode =
            TextView.makeFocusableLabel (Text.pack (show aMode))
            <&> (^. Align.tValue)
            <&> (,) aMode
        cur = Property.value prop

switchEventMap ::
    (Functor f, MonadReader env m, Config.HasConfig env) =>
    Property f AnnotationMode -> m (EventMap (f GuiState.Update))
switchEventMap (Property infoMode setAnnotationMode) =
    Lens.view (Config.config . Config.nextAnnotationModeKeys) <&>
    \keys -> E.keysEventMap keys nextDoc (setAnnotationMode next)
    where
        next = AnnotationMode.next infoMode
        nextDoc = E.Doc ["View", "Subtext", "Show " <> Text.pack (show next)]
