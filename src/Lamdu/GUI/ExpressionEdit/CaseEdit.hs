{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View, (/-/), (/|/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import           Lamdu.Calc.Type (Tag)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
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
make (Sugar.Case mArg alts caseTail addAlt _cEntityId) pl =
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
                (Widget.makeFocusableView ?? headerId)
                <*> (ExpressionGui.grammarLabel text <&> TreeLayout.fromTextView)
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
                    return (mTag, ExpressionGui.combine [argEdit, caseLabel])
        (altsGui, resultPicker) <-
            ExprGuiM.listenResultPicker $
            do
                altsGui <- makeAltsWidget mActiveTag alts myId
                case caseTail of
                    Sugar.ClosedCase deleteTail ->
                        E.weakerEvents (caseOpenEventMap config deleteTail) altsGui
                        & return
                    Sugar.CaseExtending rest ->
                        makeOpenCase rest (Widget.toAnimId myId) altsGui
        let addAltEventMap =
                addAlt
                <&> (^. Sugar.caarNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToCaseTag
                & Widget.keysEventMapMovesCursor (Config.caseAddAltKeys config)
                  (E.Doc ["Edit", "Case", "Add Alt"])
                & ExprGuiM.withHolePicker resultPicker
        ExpressionGui.addValFrame
            <*> (TreeLayout.vboxSpaced ?? [header, altsGui])
            <&> E.weakerEvents addAltEventMap
    & Widget.assignCursor myId (destCursorId alts headerId)
    & ExpressionGui.stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
        headerId = Widget.joinId myId ["header"]

makeAltRow ::
    Monad m =>
    Maybe Tag ->
    Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m (WithTextPos (Widget (Transaction m Widget.EventResult)), ExpressionGui m)
makeAltRow mActiveTag (Sugar.CaseAlt delete tag altExpr) =
    do
        config <- Lens.view Config.config
        addBg <- ExpressionGui.addValBGWithColor Theme.evaluatedPathBGColor
        let itemEventMap = caseDelEventMap config delete
        tagLabel <-
            TagEdit.makeCaseTag (ExprGuiT.nextHolesBefore altExpr) tag
            <&> Align.tValue %~ E.weakerEvents itemEventMap
            <&> if mActiveTag == Just (tag ^. Sugar.tagVal)
                then addBg
                else id
        hspace <- Spacer.stdHSpace
        altExprGui <-
            ExprGuiM.makeSubexpression altExpr <&> E.weakerEvents itemEventMap
        return (tagLabel /|/ hspace, altExprGui)

makeAltsWidget ::
    Monad m =>
    Maybe Tag ->
    [Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeAltsWidget _ [] myId =
    (Widget.makeFocusableView ?? Widget.joinId myId ["Ø"])
    <*> (ExpressionGui.grammarLabel "Ø" <&> TreeLayout.fromTextView)
makeAltsWidget mActiveTag alts _myId =
    TreeLayout.taggedList <*> mapM (makeAltRow mActiveTag) alts

separationBar :: Theme -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View.make 1
    & View.tint (Theme.caseTailColor theme)
    & View.scale (Vector2 width 10)

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
        return $ altsGui & TreeLayout.render . Lens.imapped %@~
            \layoutMode alts ->
            let restLayout = layoutMode & restExpr ^. TreeLayout.render
                minWidth = restLayout ^. View.width
                targetWidth = alts ^. View.width
            in
            alts
            /-/
            separationBar theme (max minWidth targetWidth) animId
            /-/
            vspace
            /-/
            restLayout

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
