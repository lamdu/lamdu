{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Calc.Type (Tag)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Eval.Results as ER
import           Lamdu.GUI.ExpressionEdit.Composite (destCursorId)
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

make ::
    Monad m =>
    Sugar.Case (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Case mArg (Sugar.Composite alts caseTail addAlt)) pl =
    do
        config <- Lens.view Config.config
        let mExprAfterHeader =
                ( alts ^.. Lens.traversed . Lens.traversed
                ++ caseTail ^.. Lens.traversed
                ) ^? Lens.traversed
        labelJumpHoleEventMap <-
            mExprAfterHeader <&> ExprGuiT.nextHolesBefore
            & Lens._Just ExprEventMap.jumpHolesEventMap
            <&> fromMaybe mempty
        let headerLabel text =
                (Widget.makeFocusableView ?? headerId <&> (Align.tValue %~))
                <*> ExpressionGui.grammarLabel text
                <&> Responsive.fromWithTextPos
                <&> E.weakerEvents labelJumpHoleEventMap
        (mActiveTag, header) <-
            case mArg of
            Sugar.LambdaCase -> headerLabel "λ:" <&> (,) Nothing
            Sugar.CaseWithArg (Sugar.CaseArg arg toLambdaCase) ->
                do
                    argEdit <-
                        ExprGuiM.makeSubexpression arg
                        <&> E.weakerEvents (toLambdaCaseEventMap config toLambdaCase)
                    caseLabel <- headerLabel ":"
                    mTag <-
                        ExpressionGui.evaluationResult (arg ^. Sugar.rPayload)
                        <&> (>>= (^? ER.body . ER._RInject . V.injectTag))
                    return (mTag, Options.box Options.disambiguationNone [argEdit, caseLabel])
        (altsGui, resultPicker) <-
            ExprGuiM.listenResultPicker $
            do
                altsGui <- makeAltsWidget mActiveTag alts myId
                case caseTail of
                    Sugar.ClosedComposite actions ->
                        E.weakerEvents (closedCaseEventMap config actions) altsGui
                        & return
                    Sugar.OpenComposite _actions rest ->
                        makeOpenCase rest (Widget.toAnimId myId) altsGui
        let addAltEventMap =
                addAlt
                <&> (^. Sugar.cairNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToCaseTag
                & Widget.keysEventMapMovesCursor (Config.caseAddAltKeys config)
                  (E.Doc ["Edit", "Case", "Add Alt"])
                & ExprGuiM.withHolePicker resultPicker
        ExpressionGui.addValFrame
            <*> (Responsive.vboxSpaced ?? [header, altsGui])
            <&> E.weakerEvents addAltEventMap
    & Widget.assignCursor myId headerId
    & ExpressionGui.stdWrapParentExpr pl (destCursorId alts (pl ^. Sugar.plEntityId))
    where
        myId = WidgetIds.fromExprPayload pl
        headerId = Widget.joinId myId ["header"]

makeAltRow ::
    Monad m =>
    Maybe Tag ->
    Sugar.CompositeItem (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)), ExpressionGui m)
makeAltRow mActiveTag (Sugar.CompositeItem delete tag altExpr) =
    do
        config <- Lens.view Config.config
        addBg <- ExpressionGui.addValBGWithColor Theme.evaluatedPathBGColor
        let itemEventMap = caseDelEventMap config delete
        tagLabel <-
            TagEdit.makeCaseTag TagEdit.WithTagHoles (ExprGuiT.nextHolesBefore altExpr) tag
            <&> Align.tValue %~ E.weakerEvents itemEventMap
            <&> if mActiveTag == Just (tag ^. Sugar.tagInfo . Sugar.tagVal)
                then addBg
                else id
        hspace <- Spacer.stdHSpace
        altExprGui <-
            ExprGuiM.makeSubexpression altExpr <&> E.weakerEvents itemEventMap
        return (tagLabel /|/ hspace, altExprGui)

makeAltsWidget ::
    Monad m =>
    Maybe Tag ->
    [Sugar.CompositeItem (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeAltsWidget _ [] myId =
    (Widget.makeFocusableView ?? Widget.joinId myId ["Ø"] <&> (Align.tValue %~))
    <*> ExpressionGui.grammarLabel "Ø"
    <&> Responsive.fromWithTextPos
makeAltsWidget mActiveTag alts _myId =
    Responsive.taggedList <*> mapM (makeAltRow mActiveTag) alts

separationBar :: Theme.CodeForegroundColors -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (Theme.caseTailColor theme)
    & Element.scale (Vector2 width 10)

makeOpenCase ::
    Monad m =>
    ExprGuiT.SugarExpr m -> AnimId -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
makeOpenCase rest animId altsGui =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        restExpr <-
            ExpressionGui.addValPadding
            <*> ExprGuiM.makeSubexpression rest
        return $ altsGui & Responsive.render . Lens.imapped %@~
            \layoutMode alts ->
            let restLayout = layoutMode & restExpr ^. Responsive.render
                minWidth = restLayout ^. Element.width
                targetWidth = alts ^. Element.width
            in
            alts
            /-/
            separationBar (Theme.codeForegroundColors theme) (max minWidth targetWidth) animId
            /-/
            vspace
            /-/
            restLayout

closedCaseEventMap ::
    Monad m =>
    Config -> Sugar.ClosedCompositeActions m ->
    Widget.EventMap (T m Widget.EventResult)
closedCaseEventMap config (Sugar.ClosedCompositeActions open) =
    Widget.keysEventMapMovesCursor (Config.caseOpenKeys config)
    (E.Doc ["Edit", "Case", "Open"]) $ WidgetIds.fromEntityId <$> open

caseDelEventMap ::
    Monad m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
caseDelEventMap config delete =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Case", "Delete Alt"]) $ WidgetIds.fromEntityId <$> delete

toLambdaCaseEventMap ::
    Monad m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
toLambdaCaseEventMap config toLamCase =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Case", "Turn to Lambda-Case"]) $
    WidgetIds.fromEntityId <$> toLamCase
