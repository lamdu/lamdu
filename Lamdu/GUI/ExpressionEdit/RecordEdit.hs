{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
  ( make
  ) where

import           Control.Applicative ((<$>))
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.List as List
import           Data.Monoid (Monoid(..), (<>))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

make ::
  MonadA m =>
  Sugar.Record (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make reco pl = makeUnwrapped reco & ExpressionGui.stdWrapParentExpr pl

makeFieldRow ::
  MonadA m =>
  Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiM.Payload) ->
  ExprGuiM m [ExpressionGui m]
makeFieldRow (Sugar.RecordField mDelete tag fieldExpr) = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  fieldRefGui <- TagEdit.makeRecordTag (ExprGuiM.nextHolesBefore fieldExpr) tag
  fieldExprGui <- ExprGuiM.makeSubexpression 0 fieldExpr
  let
    itemEventMap = maybe mempty (recordDelEventMap config) mDelete
  space <-
    BWidgets.stdSpaceWidget & ExprGuiM.widgetEnv <&> ExpressionGui.fromValueWidget
  [ fieldRefGui & ExpressionGui.egAlignment . _1 .~ 1
    , space
    , fieldExprGui & ExpressionGui.egAlignment . _1 .~ 0
    ]
    <&> ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
    & return

makeFieldsWidget ::
  MonadA m =>
  [Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiM.Payload)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldsWidget [] myId =
  ExpressionGui.grammarLabel "Ø" (Widget.toAnimId myId)
  >>= ExpressionGui.egWidget %%~ ExprGuiM.widgetEnv . BWidgets.makeFocusableView myId
makeFieldsWidget fields _ =
  do
    vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
    mapM makeFieldRow fields
      <&> List.intersperse (replicate 3 (ExpressionGui.fromValueWidget vspace))
      <&> ExpressionGui.gridTopLeftFocal

separationBar :: Config -> Widget.R -> Anim.AnimId -> ExpressionGui m
separationBar config width animId =
  Anim.unitSquare (animId <> ["tailsep"])
  & View 1
  & Widget.fromView
  & Widget.tint (Config.recordTailColor config)
  & Widget.scale (Vector2 width 10)
  & ExpressionGui.fromValueWidget

makeOpenRecord :: MonadA m =>
  ExpressionGui m -> ExprGuiM.SugarExpr m -> AnimId ->
  ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui rest animId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
    restExpr <- ExprGuiM.makeSubexpression 0 rest <&> pad config
    let minWidth = restExpr ^. ExpressionGui.egWidget . Widget.wWidth
    return $ ExpressionGui.vboxTopFocal $
      [ fieldsGui
      , separationBar config (max minWidth targetWidth) animId
      , ExpressionGui.fromValueWidget vspace
      , restExpr
      ]
  where
    targetWidth = fieldsGui ^. ExpressionGui.egWidget . Widget.wWidth

pad :: Config -> ExpressionGui m -> ExpressionGui m
pad config = ExpressionGui.pad $ realToFrac <$> Config.valFramePadding config

makeUnwrapped ::
  MonadA m =>
  Sugar.Record (Name m) m (ExprGuiM.SugarExpr m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.Record fields recordTail mAddField) myId =
  ExprGuiM.assignCursor myId defaultPos $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    (gui, resultPickers) <-
      ExprGuiM.listenResultPickers $ do
        fieldsGui <- makeFieldsWidget fields myId <&> pad config
        case recordTail of
          Sugar.ClosedRecord mDeleteTail ->
            fieldsGui
            & ExpressionGui.egWidget %~
              Widget.weakerEvents
              (maybe mempty (recordOpenEventMap config) mDeleteTail)
            & return
          Sugar.RecordExtending rest ->
            makeOpenRecord fieldsGui rest (Widget.toAnimId myId)
    let
      addFieldEventMap Nothing = mempty
      addFieldEventMap (Just addField) =
        ExprGuiM.holePickersAction resultPickers >> addField
        <&> (^. Sugar.rafrNewTag . Sugar.tagInstance)
        <&> WidgetIds.fromEntityId
        <&> TagEdit.diveIntoRecordTag
        & Widget.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
          (E.Doc ["Edit", "Record", "Add Field"])
    gui
      & ExpressionGui.egWidget %~ Widget.weakerEvents (addFieldEventMap mAddField)
      & ExpressionGui.egWidget %%~ ExpressionGui.addValBG myId
  where
    defaultPos =
      case fields of
      [] -> myId
      (f : _) ->
        f ^. Sugar.rfExpr . Sugar.rPayload
        & WidgetIds.fromExprPayload

recordOpenEventMap ::
  MonadA m =>
  Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
recordOpenEventMap config open =
  Widget.keysEventMapMovesCursor (Config.recordOpenKeys config)
  (E.Doc ["Edit", "Record", "Open"]) $ WidgetIds.fromEntityId <$> open

recordDelEventMap ::
  MonadA m =>
  Config -> T m Sugar.EntityId -> Widget.EventHandlers (T m)
recordDelEventMap config delete =
  Widget.keysEventMapMovesCursor (Config.delKeys config)
  (E.Doc ["Edit", "Record", "Delete Field"]) $ WidgetIds.fromEntityId <$> delete
