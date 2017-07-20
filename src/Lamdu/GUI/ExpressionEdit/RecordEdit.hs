{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View, (/-/), (/|/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
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
    do
        config <- Lens.view Config.config
        (gui, resultPicker) <-
            ExprGuiM.listenResultPicker $
            do
                fieldsGui <- makeFieldsWidget fields myId
                case recordTail of
                    Sugar.ClosedRecord deleteTail ->
                        E.weakerEvents (recordOpenEventMap config deleteTail) fieldsGui
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
                & ExprGuiM.withHolePicker resultPicker
        (if addBg then ExpressionGui.addValFrame else return id)
            ?? E.weakerEvents addFieldEventMap gui
    & Widget.assignCursor myId (defaultPos fields myId)
    & ExpressionGui.stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
        addBg = shouldAddBg record

makeFieldRow ::
    Monad m =>
    Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload) ->
    ExprGuiM m
    ( WithTextPos (Widget (Transaction m Widget.EventResult))
    , ExpressionGui m
    )
makeFieldRow (Sugar.RecordField delete tag fieldExpr) =
    do
        config <- Lens.view Config.config
        let itemEventMap = recordDelEventMap config delete
        tagLabel <-
            TagEdit.makeRecordTag (ExprGuiT.nextHolesBefore fieldExpr) tag
            <&> Align.tValue %~ E.weakerEvents itemEventMap
        hspace <- Spacer.stdHSpace
        fieldGui <- ExprGuiM.makeSubexpression fieldExpr
        return (tagLabel /|/ hspace, E.weakerEvents itemEventMap fieldGui)

makeFieldsWidget ::
    Monad m =>
    [Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldsWidget [] myId =
    (Widget.makeFocusableView ?? myId)
    <*> (ExpressionGui.grammarLabel "()" <&> TreeLayout.fromTextView)
makeFieldsWidget fields _ =
    TreeLayout.taggedList <*> mapM makeFieldRow fields

separationBar :: Theme -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    Anim.unitSquare (animId <> ["tailsep"])
    & View.make 1
    & View.tint (Theme.recordTailColor theme)
    & View.scale (Vector2 width 10)

makeOpenRecord ::
    Monad m =>
    ExpressionGui m -> ExprGuiT.SugarExpr m -> AnimId ->
    ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui rest animId =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        restExpr <-
            ExpressionGui.addValPadding
            <*> ExprGuiM.makeSubexpression rest
        return $ fieldsGui & TreeLayout.render . Lens.imapped %@~
            \layoutMode fields ->
            let restW = (restExpr ^. TreeLayout.render) layoutMode
                minWidth = restW ^. View.width
                targetWidth = fields ^. View.width
            in
            fields
            /-/
            separationBar theme (max minWidth targetWidth) animId
            /-/
            vspace
            /-/
            restW

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
