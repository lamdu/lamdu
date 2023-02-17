-- | DropDownList widget for presentation mode
{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.PresentationModeEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property, pVal)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Data.Meta (_Operator)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

lens :: Sugar.SpecialArgs dummy -> Lens.Lens' (Texts.CodeUI a) a
lens mode =
    case mode of
    Sugar.Verbose -> Texts.pModeVerbose
    Sugar.Operator{} -> Texts.pModeOperator

{-# ANN make ("HLint: ignore Use head"::String) #-}
make ::
    _ =>
    ElemId ->
    Sugar.LhsNames v name i o ->
    Property f Sugar.PresentationMode ->
    m (Align.TextWidget f)
make myId (Sugar.LhsRecord params) prop =
    do
        theme <- Lens.view has
        pairs <-
            traverse mkPair
            [ Sugar.Verbose
            , prop ^? pVal . _Operator & maybe (Sugar.Operator (paramTags !! 0) (paramTags !! 1)) (_Operator #)
            ]
            & local
                (has . TextView.styleColor .~ theme ^. Theme.textColors . TextColors.presentationChoiceColor)
        defConfig <-
            DropDownList.defaultConfig
            <*> Lens.view (has . Texts.presentationMode)
        DropDownList.make ?? prop ?? pairs
            ?? defConfig ?? myId
            <&> Element.scale (theme ^. Theme.presentationChoiceScaleFactor)
    where
        paramTags = params ^.. SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagVal
        mkPair mode = Styled.focusableLabel (lens mode) <&> (,) mode
make _ _ _ =
    -- This shouldn't happen?
    pure Element.empty
