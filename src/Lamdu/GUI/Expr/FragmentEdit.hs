{-# LANGUAGE TypeOperators #-}
module Lamdu.GUI.Expr.FragmentEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Hyper (Ann(..), type (#))
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.Expr.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (parentDelegator)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

fragmentDoc ::
    ( Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Lens.ALens' env Text -> E.Doc
fragmentDoc env lens =
    E.toDoc env
    [has . MomentuTexts.edit, has . Texts.fragment, lens]

make ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , Has (TextEdit.Texts Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Navigation Text) env
    , SearchMenu.HasTexts env
    ) =>
    Sugar.Fragment Name i o # Ann (Const (Sugar.Payload Name i o ExprGui.Payload)) ->
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
make fragment pl =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        isHoleResult <- GuiM.isHoleResult
        env <- Lens.view id

        fragmentExprGui <- fragment ^. Sugar.fExpr & GuiM.makeSubexpression

        addAnnotation <- maybeAddAnnotationPl pl

        hbox <- ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl

        searchArea <-
            SearchArea.make SearchArea.WithoutAnnotation
            (fragment ^. Sugar.fOptions) pl allowedFragmentSearchTerm
            ?? Menu.AnyPlace

        qmark <-
            (Element.padToSize ?? searchArea ^. Align.tValue . Widget.wSize ?? 0)
            <*>
            ( (Widget.makeFocusableView ?? closedHoleId <&> (Align.tValue %~))
                <*> Label.make "?"
            ) <&> Responsive.fromWithTextPos

        ExprEventMap.add ExprEventMap.defaultOptions pl
            <*>
            ( parentDelegator myId
                ?? hbox
                [ fragmentExprGui
                , if isSelected && not isHoleResult
                    then Responsive.fromWithTextPos searchArea
                    else qmark
                ]
            )
            <&> Widget.widget %~ addAnnotation
            <&> Widget.widget %~ Widget.weakerEvents (healEventMap env)
            <&> Widget.widget . Widget.wState . Widget._StateUnfocused .
                Widget.uMStroll .~
                ((strollDest ^. Lens._Unwrapped, strollDest ^. Lens._Unwrapped)
                    <$ guard (not isHoleResult))
    where
        myId = WidgetIds.fromExprPayload pl
        holeIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        closedHoleId = HoleWidgetIds.hidClosed holeIds
        strollDest = HoleWidgetIds.hidOpen holeIds
        healEventMap env =
            fragment ^. Sugar.fHeal <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor
                (Config.delKeys env <> env ^. has . Config.healKeys)
                (fragmentDoc env (has . Texts.heal))
