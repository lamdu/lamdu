{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
    ( make
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
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

import           Lamdu.Prelude

defaultPos ::
    [Sugar.RecordField name m (Sugar.Expression name m a)] ->
    Widget.Id -> Widget.Id
defaultPos [] myId = myId
defaultPos (f : _) _ =
    f ^. Sugar.rfExpr . Sugar.rPayload & WidgetIds.fromExprPayload

shouldAddBg :: Sugar.Record name m a -> Bool
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
                    (if addBg then ExpressionGui.addValPadding else return id)
                    <*>  makeFieldsWidget fields myId
                case recordTail of
                    Sugar.ClosedRecord deleteTail ->
                        fieldsGui
                        & TreeLayout.widget %~
                          Widget.weakerEvents (recordOpenEventMap config deleteTail)
                        & return
                    Sugar.RecordExtending rest ->
                        makeOpenRecord fieldsGui rest (Widget.toAnimId myId)
        let addFieldEventMap =
                addField
                <&> (^. Sugar.rafrNewTag . Sugar.tagInstance)
                <&> WidgetIds.fromEntityId
                <&> TagEdit.diveToRecordTag
                & Widget.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
                  (E.Doc ["Edit", "Record", "Add Field"])
                & ExprGuiM.withHolePickers resultPickers
        gui
            & TreeLayout.widget %~ Widget.weakerEvents addFieldEventMap
            & if addBg
                then
                    (<*>)
                    (ExpressionGui.addValBG myId <&> (TreeLayout.widget %~))
                    . return
                else return
    where
        addBg = shouldAddBg record

makeFieldRow ::
    Monad m =>
    Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m (ExpressionGui m)
makeFieldRow (Sugar.RecordField delete tag fieldExpr) =
    do
        config <- ExprGuiM.readConfig
        fieldRefGui <-
            TagEdit.makeRecordTag (ExprGuiT.nextHolesBefore fieldExpr) tag
        fieldExprGui <- ExprGuiM.makeSubexpression (const 0) fieldExpr
        let itemEventMap = recordDelEventMap config delete
        ExpressionGui.tagItem ?? fieldRefGui ?? fieldExprGui
            <&> TreeLayout.widget %~ Widget.weakerEvents itemEventMap

makeFieldsWidget ::
    Monad m =>
    [Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldsWidget [] myId =
    ExpressionGui.makeFocusableView myId
    <*> ExpressionGui.grammarLabel "()" (Widget.toAnimId myId)
    <&> TreeLayout.fromAlignedWidget
makeFieldsWidget fields _ =
    ExpressionGui.vboxTopFocalSpaced <*> mapM makeFieldRow fields

separationBar :: Config -> Widget.R -> Anim.AnimId -> View
separationBar config width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View.make 1
    & View.tint (Config.recordTailColor config)
    & View.scale (Vector2 width 10)

makeOpenRecord ::
    Monad m =>
    ExpressionGui m -> ExprGuiT.SugarExpr m -> AnimId ->
    ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui rest animId =
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
                fields = layoutMode & fieldsGui ^. TreeLayout.render
                targetWidth = fields ^. AlignedWidget.widget . Widget.width
            in
            fields
            & AlignedWidget.alignment . _1 .~ 0
            & AlignedWidget.addAfter AlignedWidget.Vertical
            ( [ separationBar config (max minWidth targetWidth) animId
                & Widget.fromView
                & AlignedWidget.fromCenteredWidget
              , AlignedWidget.fromCenteredWidget vspace
              , restLayout
              ] <&> AlignedWidget.alignment . _1 .~ 0
            )

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
