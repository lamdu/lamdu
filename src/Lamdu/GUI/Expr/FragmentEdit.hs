{-# LANGUAGE TypeOperators #-}
module Lamdu.GUI.Expr.FragmentEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as ValAnnotation
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.Expr.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Annotation
                 (maybeAddAnnotationPlWith, WhichAnnotation(..))
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

        addAnnotation <- maybeAddAnnotationPlWith postProcessAnn pl

        rawSearchArea <-
            SearchArea.make SearchArea.WithoutAnnotation
            (fragment ^. Sugar.fOptions) pl allowedFragmentSearchTerm holeIds
            ?? Menu.AnyPlace

        qmarkView <-
            (Element.padToSize ?? rawSearchArea ^. Align.tValue . Widget.wSize ?? 0.5)
            <*> Label.make "?"

        searchArea <-
            Element.padToSize ?? qmarkView ^. Align.tValue . View.vSize ?? 0.5
            <&> (Align.tValue %~)
            ?? rawSearchArea
            <&> Lens.mapped %~
                Widget.weakerEvents (healEventMap (Config.delKeys env) env)
        let qmarkImage = qmarkView ^. Align.tValue . View.vAnimLayers
        let searchAreaQMark = searchArea <&> Element.setLayeredImage .~ qmarkImage
        let healKeys = env ^. has . Config.healKeys

        hbox <- ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl

        ExprEventMap.add ExprEventMap.defaultOptions pl
            <*>
            ( parentDelegator myId
                ?? hbox
                [ fragmentExprGui
                , Responsive.fromWithTextPos $
                    if isSelected && not isHoleResult
                    then searchArea
                    else searchAreaQMark
                ]
            )
            <&> Widget.widget %~ addAnnotation
            <&> Widget.weakerEvents (healEventMap healKeys env)
    where
        lineAbove color animId spacing ann =
            ann
            & Element.setLayeredImage . Element.layers %~ (<> [line])
            where
                line =
                    Anim.coloredRectangle animId color
                    & Anim.scale (Vector2 (ann ^. View.vSize . _1) (-spacing))

        postProcessAnn TypeAnnotation
            | not (fragment ^. Sugar.fTypeMatch) =
            do
                color <- Lens.view (has . Theme.errorColor)
                animId <- Element.subAnimId ?? ["err-line"]
                spacing <- Lens.view
                    (has . Theme.valAnnotation . ValAnnotation.valAnnotationSpacing)
                stdFontHeight <- Spacer.stdFontHeight
                lineAbove color animId (spacing * stdFontHeight) & pure
        postProcessAnn _ = pure id

        myId = WidgetIds.fromExprPayload pl
        holeIds = WidgetIds.fragmentHoleId myId & HoleWidgetIds.makeFrom
        healEventMap keys env =
            fragment ^. Sugar.fHeal <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor keys (fragmentDoc env (has . Texts.heal))
