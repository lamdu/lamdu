-- | Choice widget for presentation mode

module Lamdu.GUI.PresentationModeEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Property (Property)
import qualified Data.Text as Text
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

{-# ANN make ("HLint: ignore Use head"::String) #-}

make ::
    ( Applicative f, MonadReader env m, HasTheme env
    , Element.HasAnimIdPrefix env, TextView.HasStyle env, GuiState.HasCursor env
    , Hover.HasStyle env, Element.HasLayoutDir env
    ) =>
    Widget.Id ->
    Sugar.BinderParams name i o ->
    Property f Sugar.PresentationMode ->
    m (Gui Widget f)
make myId (Sugar.Params params) prop =
    do
        theme <- Lens.view Theme.theme
        pairs <-
            traverse mkPair [Sugar.Object (paramTags !! 0), Sugar.Verbose, Sugar.Infix (paramTags !! 0) (paramTags !! 1)]
            & Reader.local
                (TextView.style . TextView.styleColor .~ theme ^. Theme.textColors . TextColors.presentationChoiceColor)
        Choice.make ?? prop ?? pairs
            ?? Choice.defaultConfig "Presentation Mode" ?? myId
            <&> Element.scale (theme ^. Theme.presentationChoiceScaleFactor)
    where
        paramTags = params ^.. traverse . Sugar.fpInfo . Sugar.piTag . Sugar.tagInfo . Sugar.tagVal
        mkPair presentationMode =
            Label.makeFocusable text <&> (^. Align.tValue)
            <&> (,) presentationMode
            where
                text =
                    case presentationMode of
                    Sugar.Verbose -> "Verbose"
                    Sugar.Object{} -> "OO"
                    Sugar.Infix{} -> "Infix"
                    & Text.pack
make _ _ _ =
    -- This shouldn't happen?
    pure Element.empty
