{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import           Lamdu.Calc.Type (Tag)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Eval.Results as ER
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

destCursorId ::
    [Sugar.CaseAlt name n (Sugar.Expression name n p)] ->
    Widget.Id -> Widget.Id
destCursorId [] defDestId = defDestId
destCursorId (alt : _) _ =
    alt ^. Sugar.caHandler . Sugar.rPayload & WidgetIds.fromExprPayload

make ::
    Monad m =>
    Sugar.Case (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Case mArg alts caseTail addAlt cEntityId) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    let headerId = Widget.joinId myId ["header"]
    in ExprGuiM.assignCursor myId (destCursorId alts headerId) $
    do
        config <- ExprGuiM.readConfig
        let mExprAfterHeader =
                ( alts ^.. Lens.traversed . Lens.traversed
                ++ caseTail ^.. Lens.traversed
                ) ^? Lens.traversed
        labelJumpHoleEventMap <-
            mExprAfterHeader <&> ExprGuiT.nextHolesBefore
            & Lens._Just ExprEventMap.jumpHolesEventMap
            <&> fromMaybe mempty
        let headerLabel text =
                ExpressionGui.makeFocusableView headerId
                <*>
                (ExpressionGui.grammarLabel text
                    (Widget.toAnimId (WidgetIds.fromEntityId cEntityId))
                    <&> AlignedWidget.widget
                        %~ Widget.weakerEvents labelJumpHoleEventMap
                )
                <&> TreeLayout.fromAlignedWidget
        (mActiveTag, header) <-
            case mArg of
            Sugar.LambdaCase -> headerLabel "λ:" <&> (,) Nothing
            Sugar.CaseWithArg (Sugar.CaseArg arg toLambdaCase) ->
                do
                    argEdit <-
                        ExprGuiM.makeSubexpression (const 0) arg
                        <&> TreeLayout.widget %~ Widget.weakerEvents
                            (toLambdaCaseEventMap config toLambdaCase)
                    caseLabel <- headerLabel ":"
                    mTag <-
                        ExpressionGui.evaluationResult (arg ^. Sugar.rPayload)
                        <&> (>>= (^? ER.body . ER._RInject . V.injectTag))
                    return (mTag, ExpressionGui.combine [argEdit, caseLabel])
        (altsGui, resultPickers) <-
            ExprGuiM.listenResultPickers $
            do
                altsGui <- makeAltsWidget mActiveTag alts myId
                case caseTail of
                    Sugar.ClosedCase deleteTail ->
                        altsGui
                        & TreeLayout.widget %~
                          Widget.weakerEvents
                          (caseOpenEventMap config deleteTail)
                        & return
                    Sugar.CaseExtending rest ->
                        altsGui
                        & makeOpenCase rest (Widget.toAnimId myId)
        let addAltEventMap =
                addAlt
                <&> (^. Sugar.caarNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToCaseTag
                & Widget.keysEventMapMovesCursor (Config.caseAddAltKeys config)
                  (E.Doc ["Edit", "Case", "Add Alt"])
                & ExprGuiM.withHolePickers resultPickers
        ExpressionGui.addValFrame myId
            <*> (ExpressionGui.vboxTopFocalSpaced ??
                 ([header, altsGui] <&> TreeLayout.alignment . _1 .~ 0))
            <&> TreeLayout.widget %~ Widget.weakerEvents addAltEventMap

makeAltRow ::
    Monad m =>
    Maybe Tag ->
    Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m (ExpressionGui m)
makeAltRow mActiveTag (Sugar.CaseAlt delete tag altExpr) =
    do
        config <- ExprGuiM.readConfig
        addBg <-
            ExpressionGui.addValBGWithColor Config.evaluatedPathBGColor
            (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
        altRefGui <-
            TagEdit.makeCaseTag (ExprGuiT.nextHolesBefore altExpr) tag
            <&> if mActiveTag == Just (tag ^. Sugar.tagVal)
                then AlignedWidget.widget %~ addBg
                else id
        altExprGui <- ExprGuiM.makeSubexpression (const 0) altExpr
        let itemEventMap = caseDelEventMap config delete
        ExpressionGui.tagItem ?? altRefGui ?? altExprGui
            <&> TreeLayout.widget %~ Widget.weakerEvents itemEventMap

makeAltsWidget ::
    Monad m =>
    Maybe Tag ->
    [Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeAltsWidget _ [] myId =
    ExpressionGui.makeFocusableView (Widget.joinId myId ["Ø"])
    <*> ExpressionGui.grammarLabel "Ø" (Widget.toAnimId myId)
    <&> TreeLayout.fromAlignedWidget
makeAltsWidget mActiveTag alts _ =
    ExpressionGui.vboxTopFocalSpaced <*> mapM (makeAltRow mActiveTag) alts

separationBar :: Config -> Widget.R -> Anim.AnimId -> View
separationBar config width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View.make 1
    & View.tint (Config.caseTailColor config)
    & View.scale (Vector2 width 10)

makeOpenCase ::
    Monad m =>
    ExprGuiT.SugarExpr m -> AnimId -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
makeOpenCase rest animId altsGui =
    do
        config <- ExprGuiM.readConfig
        vspace <- ExpressionGui.stdVSpace
        restExpr <-
            ExpressionGui.addValPadding
            <*> ExprGuiM.makeSubexpression (const 0) rest
        return $ TreeLayout.render #
            \layoutMode ->
            let restLayout = layoutMode & restExpr ^. TreeLayout.render
                minWidth = restLayout ^. AlignedWidget.widget . Widget.width
                alts = layoutMode & altsGui ^. TreeLayout.render
                targetWidth = alts ^. AlignedWidget.widget . Widget.width
            in
            alts
            & AlignedWidget.alignment . _1 .~ 0
            & AlignedWidget.addAfter AlignedWidget.Vertical
            ( [ separationBar config (max minWidth targetWidth) animId
                & Widget.fromView & AlignedWidget.fromCenteredWidget
              , AlignedWidget.fromCenteredWidget vspace
              , restLayout
              ] <&> (AlignedWidget.alignment . _1 .~ 0)
            )

caseOpenEventMap ::
    Monad m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
caseOpenEventMap config open =
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
