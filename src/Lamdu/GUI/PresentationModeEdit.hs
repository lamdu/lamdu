-- | Choice widget for presentation mode
{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.PresentationModeEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Property (Property)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.Styled (OneOfT(..))
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

lens :: Sugar.SpecialArgs dummy -> Lens.Lens' (Texts.CodeUI a) a
lens mode =
    case mode of
    Sugar.Verbose -> Texts.pModeVerbose
    Sugar.Operator{} -> Texts.pModeOperator

{-# ANN make ("HLint: ignore Use head"::String) #-}
make ::
    ( Applicative f, MonadReader env m, Has Theme env
    , Element.HasAnimIdPrefix env, Has TextView.Style env, GuiState.HasCursor env
    , Has Hover.Style env
    , Has (Texts.CodeUI Text) env
    , Has (Choice.Texts Text) env
    , Glue.HasTexts env
    ) =>
    Widget.Id ->
    Sugar.BinderParams v name i o ->
    Property f Sugar.PresentationMode ->
    m (Align.TextWidget f)
make myId (Sugar.Params params) prop =
    do
        theme <- Lens.view has
        pairs <-
            traverse mkPair [Sugar.Verbose, Sugar.Operator (paramTags !! 0) (paramTags !! 1)]
            & Reader.local
                (has . TextView.styleColor .~ theme ^. Theme.textColors . TextColors.presentationChoiceColor)
        defConfig <-
            Choice.defaultConfig
            <*> Lens.view (has . Texts.presentationMode)
        Choice.make ?? prop ?? pairs
            ?? defConfig ?? myId
            <&> Element.scale (theme ^. Theme.presentationChoiceScaleFactor)
    where
        paramTags =
            params ^.. traverse . _2 . Sugar.piTag . Sugar.tagRefTag . Sugar.tagVal
        mkPair mode = Styled.mkFocusableLabel ?? OneOf (lens mode) <&> (,) mode
make _ _ _ =
    -- This shouldn't happen?
    pure Element.empty
