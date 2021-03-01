module Lamdu.GUI.Expr.FragmentEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as ValAnnotation
import qualified Lamdu.GUI.Expr.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.Expr.HoleEdit.ValTerms (allowedSearchTerm)
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.Annotation (addInferredType, shrinkValAnnotationsIfNeeded)
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

fragmentDoc :: _ => env -> Lens.ALens' env Text -> E.Doc
fragmentDoc env lens =
    E.toDoc env
    [has . MomentuTexts.edit, has . Texts.fragment, lens]

make :: _ => ExprGui.Expr Sugar.Fragment i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) fragment) =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        isHoleResult <- GuiM.isHoleResult
        env <- Lens.view id

        fragmentExprGui <- fragment ^. Sugar.fExpr & GuiM.makeSubexpression

        rawSearchArea <-
            SearchArea.make SearchArea.WithoutAnnotation
            (fragment ^. Sugar.fOptions . Sugar.holeOptions) pl allowedSearchTerm holeIds
            ?? Menu.AnyPlace

        qmarkView <-
            (Element.padToSize ?? rawSearchArea ^. M.tValue . Widget.wSize ?? 0.5)
            <*> Label.make "?"

        searchArea <-
            Element.padToSize ?? qmarkView ^. M.tValue . View.vSize ?? 0.5
            <&> (M.tValue %~)
            ?? rawSearchArea
            <&> Lens.mapped %~
                Widget.weakerEvents (healEventMap (Config.delKeys env) "" env)
        let qmarkImage = qmarkView ^. M.tValue . View.vAnimLayers
        let searchAreaQMark = searchArea <&> Element.setLayeredImage .~ qmarkImage
        let healKeys = env ^. has . Config.healKeys
        let healChars =
                case env ^. has of
                Dir.LeftToRight -> ")]"
                Dir.RightToLeft -> "(["
        hbox <- ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl

        addInnerType <-
            case fragment ^. Sugar.fTypeMismatch of
            Nothing -> pure id
            Just mismatchedType ->
                do
                    color <- Lens.view (has . Theme.errorColor)
                    animId <- Element.subAnimId ?? ["err-line"]
                    spacing <- Lens.view
                        (has . Theme.valAnnotation . ValAnnotation.valAnnotationSpacing)
                    stdFontHeight <- Spacer.stdFontHeight
                    addInferredType mismatchedType shrinkValAnnotationsIfNeeded
                        <&> (lineBelow color animId (spacing * stdFontHeight) .)
            & Element.locallyAugmented ("inner type"::Text)
        hbox
            [ fragmentExprGui
            , Responsive.fromWithTextPos $
                if isSelected && not isHoleResult
                then searchArea
                else searchAreaQMark
            ]
            & Widget.widget %~ addInnerType
            & pure & stdWrapParentExpr pl
            <&> Widget.weakerEvents (healEventMap healKeys healChars env)
    where
        lineBelow color animId spacing ann =
            ann
            & Element.setLayeredImage . Element.layers . Lens.ix 0 %~ (<> line)
            where
                line =
                    Anim.coloredRectangle animId color
                    & Anim.scale (M.Vector2 (ann ^. Element.width) spacing)
                    & Anim.translate (M.Vector2 0 (ann ^. Element.height))

        myId = WidgetIds.fromExprPayload (pl ^. _1)
        holeIds = WidgetIds.fragmentHoleId myId & HoleWidgetIds.makeFrom
        healEventMap keys chars env =
            E.keysEventMapMovesCursor keys doc action <>
            E.charGroup (Just "Close Paren") doc chars
            (const (action <&> GuiState.updateCursor))
            where
                action = fragment ^. Sugar.fHeal <&> WidgetIds.fromEntityId
                doc = fragmentDoc env (has . Texts.heal)
