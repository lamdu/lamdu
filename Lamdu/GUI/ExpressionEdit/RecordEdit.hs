{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
    ( make
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

defaultPos ::
    [Sugar.RecordField name m (Sugar.Expression name m a)] ->
    Widget.Id -> Widget.Id
defaultPos [] myId = myId
defaultPos (f : _) _ =
    f ^. Sugar.rfExpr . Sugar.rPayload & WidgetIds.fromExprPayload

shouldAddBg :: Monad m => Sugar.Record name m a -> Bool
shouldAddBg (Sugar.Record [] Sugar.ClosedRecord{} _) = False
shouldAddBg _ = True

make ::
    Monad m =>
    Sugar.Record (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make record@(Sugar.Record fields recordTail addField) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    ExprGuiM.assignCursor myId (defaultPos fields myId) $
    do
        config <- ExprGuiM.readConfig
        (gui, resultPickers) <-
            ExprGuiM.listenResultPickers $
            do
                fieldsGui <-
                    makeFieldsWidget fields myId
                    >>= if addBg then ExpressionGui.addValPadding else return
                case recordTail of
                    Sugar.ClosedRecord deleteTail ->
                        fieldsGui
                        & ExpressionGui.egWidget %~
                          Widget.weakerEvents (recordOpenEventMap config deleteTail)
                        & return
                    Sugar.RecordExtending rest ->
                        makeOpenRecord fieldsGui rest (Widget.toAnimId myId)
        let addFieldEventMap =
                ExprGuiM.holePickersAction resultPickers >> addField
                <&> (^. Sugar.rafrNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToRecordTag
                & Widget.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
                  (E.Doc ["Edit", "Record", "Add Field"])
        gui
            & ExpressionGui.egWidget %~ Widget.weakerEvents addFieldEventMap
            & if addBg
                then ExpressionGui.egWidget %%~ ExpressionGui.addValBG myId
                else return
    where
        addBg = shouldAddBg record

makeFieldRow ::
    Monad m =>
    Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m [ExpressionGui m]
makeFieldRow (Sugar.RecordField delete tag fieldExpr) =
    do
        config <- ExprGuiM.readConfig
        fieldRefGui <-
            TagEdit.makeRecordTag (ExprGuiT.nextHolesBefore fieldExpr) tag
        fieldExprGui <- ExprGuiM.makeSubexpression (const 0) fieldExpr
        let itemEventMap = recordDelEventMap config delete
        space <- ExpressionGui.stdHSpace
        [ fieldRefGui & ExpressionGui.egAlignment . _1 .~ 1
            , space
            , fieldExprGui & ExpressionGui.egAlignment . _1 .~ 0
            ]
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
            & return

makeFieldsWidget ::
    Monad m =>
    [Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldsWidget [] myId =
    ExpressionGui.grammarLabel "()" (Widget.toAnimId myId)
    >>= ExpressionGui.makeFocusableView myId
makeFieldsWidget fields _ =
    do
        vspace <- ExpressionGui.stdVSpace
        mapM makeFieldRow fields
            <&> List.intersperse (replicate 3 vspace)
            <&> ExpressionGui.gridTopLeftFocal

separationBar :: Config -> Widget.R -> Anim.AnimId -> ExpressionGui m
separationBar config width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View 1
    & Widget.fromView
    & Widget.tint (Config.recordTailColor config)
    & Widget.scale (Vector2 width 10)
    & ExpressionGui.fromValueWidget

makeOpenRecord ::
    Monad m =>
    ExpressionGui m -> ExprGuiT.SugarExpr m -> AnimId ->
    ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui rest animId =
    do
        config <- ExprGuiM.readConfig
        vspace <- ExpressionGui.stdVSpace
        restExpr <-
            ExprGuiM.makeSubexpression (const 0) rest
            >>= ExpressionGui.addValPadding
        let minWidth = restExpr ^. ExpressionGui.egWidget . Widget.width
        [ fieldsGui
            , separationBar config (max minWidth targetWidth) animId
            , vspace
            , restExpr
            ] & ExpressionGui.vboxTopFocalAlignedTo 0 & return
    where
        targetWidth = fieldsGui ^. ExpressionGui.egWidget . Widget.width

recordOpenEventMap ::
    Monad m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
recordOpenEventMap config open =
    Widget.keysEventMapMovesCursor (Config.recordOpenKeys config)
    (E.Doc ["Edit", "Record", "Open"]) $ WidgetIds.fromEntityId <$> open

recordDelEventMap ::
    Monad m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
recordDelEventMap config delete =
    Widget.keysEventMapMovesCursor (Config.delKeys config)
    (E.Doc ["Edit", "Record", "Delete Field"]) $ WidgetIds.fromEntityId <$> delete
