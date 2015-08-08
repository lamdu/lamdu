{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Eval.Val as EV
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Val as V
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

type T = Transaction

assignCursor ::
    MonadA m =>
    Widget.Id ->
    [Sugar.CaseAlt name n (Sugar.Expression name n p)] ->
    ExprGuiM m a -> ExprGuiM m a
assignCursor _ [] = id
assignCursor myId (alt : _) =
    ExprGuiM.assignCursor myId
    ( alt ^. Sugar.caHandler . Sugar.rPayload
      & WidgetIds.fromExprPayload )

make ::
    MonadA m =>
    Sugar.Case (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Case mArg alts caseTail mAddAlt cEntityId) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    assignCursor myId alts $
    do
        config <- ExprGuiM.readConfig
        let mExprAfterHeader =
                ( (alts ^.. Lens.traversed . Lens.traversed)
                    ++ (caseTail ^.. Lens.traversed)
                ) ^? Lens.traversed
        labelJumpHoleEventMap <-
            mExprAfterHeader <&> ExprGuiT.nextHolesBefore
            & Lens._Just ExprEventMap.jumpHolesEventMap
            <&> fromMaybe mempty
        let label text =
                WidgetIds.fromEntityId cEntityId & Widget.toAnimId
                & ExpressionGui.grammarLabel text
        let headerLabel text =
                label text
                >>= ExpressionGui.makeFocusableView
                    (Widget.joinId myId ["header"])
                <&> ExpressionGui.egWidget
                    %~ Widget.weakerEvents labelJumpHoleEventMap
        (mActiveTag, header) <-
            case mArg of
            Sugar.LambdaCase -> headerLabel "λ:" <&> (,) Nothing
            Sugar.CaseWithArg (Sugar.CaseArg arg mToLambdaCase) ->
                do
                    argEdit <-
                        ExprGuiM.makeSubexpression 0 arg
                        <&> ExpressionGui.egWidget %~ Widget.weakerEvents
                            (maybe mempty (toLambdaCaseEventMap config)
                                mToLambdaCase)
                    caseLabel <- headerLabel ":"
                    mTag <-
                        ExpressionGui.evaluationResult (arg ^. Sugar.rPayload)
                        <&> (>>= (^? Lens._Right . EV._HInject . V.injectTag))
                    return (mTag, ExpressionGui.hbox [argEdit, caseLabel])
        (altsGui, resultPickers) <-
            ExprGuiM.listenResultPickers $
            do
                altsGui <- makeAltsWidget mActiveTag alts myId
                case caseTail of
                    Sugar.ClosedCase mDeleteTail ->
                        altsGui
                        & ExpressionGui.egWidget %~
                          Widget.weakerEvents
                          (maybe mempty (caseOpenEventMap config) mDeleteTail)
                        & return
                    Sugar.CaseExtending rest ->
                        altsGui
                        & makeOpenCase rest (Widget.toAnimId myId)
        let addAltEventMap Nothing = mempty
            addAltEventMap (Just addAlt) =
                ExprGuiM.holePickersAction resultPickers >> addAlt
                <&> (^. Sugar.caarNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToCaseTag
                & Widget.keysEventMapMovesCursor (Config.caseAddAltKeys config)
                  (E.Doc ["Edit", "Case", "Add Alt"])
        vspace <- ExpressionGui.verticalSpace
        [header, vspace, altsGui]
            & ExpressionGui.vboxTopFocalAlignedTo 0
            & pad config
            & ExpressionGui.egWidget %~
              Widget.weakerEvents (addAltEventMap mAddAlt)
            & ExpressionGui.egWidget %%~ ExpressionGui.addValBG myId

makeAltRow ::
    MonadA m =>
    Maybe Tag ->
    Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m [ExpressionGui m]
makeAltRow mActiveTag (Sugar.CaseAlt mDelete tag altExpr) =
    do
        config <- ExprGuiM.readConfig
        altRefGui <-
            TagEdit.makeCaseTag (ExprGuiT.nextHolesBefore altExpr) tag
            >>= if mActiveTag == Just (tag ^. Sugar.tagVal)
                then ExpressionGui.addValFrame
                    (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
                else return
        altExprGui <- ExprGuiM.makeSubexpression 0 altExpr
        let itemEventMap = maybe mempty (caseDelEventMap config) mDelete
        space <- ExpressionGui.stdSpace
        [ altRefGui & ExpressionGui.egAlignment . _1 .~ 1
            , space
            , altExprGui & ExpressionGui.egAlignment . _1 .~ 0
            ]
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
            & return

makeAltsWidget ::
    MonadA m =>
    Maybe Tag ->
    [Sugar.CaseAlt (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeAltsWidget _ [] myId =
    ExpressionGui.grammarLabel "Ø" (Widget.toAnimId myId)
    >>= ExpressionGui.makeFocusableView (Widget.joinId myId ["Ø"])
makeAltsWidget mActiveTag alts _ =
    do
        vspace <- ExpressionGui.verticalSpace
        mapM (makeAltRow mActiveTag) alts
            <&> List.intersperse (replicate 3 vspace)
            <&> ExpressionGui.gridTopLeftFocal

separationBar :: Config -> Widget.R -> Anim.AnimId -> ExpressionGui m
separationBar config width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View 1
    & Widget.fromView
    & Widget.tint (Config.caseTailColor config)
    & Widget.scale (Vector2 width 10)
    & ExpressionGui.fromValueWidget

makeOpenCase ::
    MonadA m =>
    ExprGuiT.SugarExpr m -> AnimId -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
makeOpenCase rest animId altsGui =
    do
        config <- ExprGuiM.readConfig
        vspace <- ExpressionGui.verticalSpace
        restExpr <- ExprGuiM.makeSubexpression 0 rest <&> pad config
        let minWidth = restExpr ^. ExpressionGui.egWidget . Widget.width
        [ altsGui
            , separationBar config (max minWidth targetWidth) animId
            , vspace
            , restExpr
            ] & ExpressionGui.vboxTopFocal & return
    where
        targetWidth = altsGui ^. ExpressionGui.egWidget . Widget.width

pad :: Config -> ExpressionGui m -> ExpressionGui m
pad config = ExpressionGui.pad $ realToFrac <$> Config.valFramePadding config

caseOpenEventMap ::
    MonadA m =>
    Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
caseOpenEventMap config open =
    Widget.keysEventMapMovesCursor (Config.caseOpenKeys config)
    (E.Doc ["Edit", "Case", "Open"]) $ WidgetIds.fromEntityId <$> open

caseDelEventMap ::
    MonadA m =>
    Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
caseDelEventMap config delete =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Case", "Delete Alt"]) $ WidgetIds.fromEntityId <$> delete

toLambdaCaseEventMap ::
    MonadA m =>
    Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
toLambdaCaseEventMap config toLamCase =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Case", "Turn to Lambda-Case"]) $
    WidgetIds.fromEntityId <$> toLamCase
