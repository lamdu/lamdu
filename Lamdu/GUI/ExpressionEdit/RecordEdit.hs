{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
  ( make
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

make ::
  MonadA m =>
  Sugar.Record (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make reco pl = ExpressionGui.stdWrapParentExpr pl $ makeUnwrapped reco

makeFieldRow ::
  MonadA m =>
  Sugar.RecordField (Name m) m (Sugar.Expression (Name m) m ExprGuiM.Payload) ->
  ExprGuiM m [(Widget.R, ExpressionGui m)]
makeFieldRow (Sugar.RecordField mDelete tag fieldExpr) = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  fieldRefGui <-
    TagEdit.makeRecordTag (ExprGuiM.nextHolesBefore fieldExpr)
    tag (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
  fieldExprGui <- ExprGuiM.makeSubexpression 0 fieldExpr
  let
    itemEventMap = maybe mempty (recordDelEventMap config) mDelete
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget
  [(1, fieldRefGui), (0.5, space), (0, fieldExprGui)]
    <&> Lens._2 . ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
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
      <&> List.intersperse (replicate 3 (0.5, ExpressionGui.fromValueWidget vspace))
      <&> ExpressionGui.gridDownwards

makeOpenRecord :: MonadA m =>
  ExpressionGui m -> ExprGuiM.SugarExpr m -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui rest myId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
    restExpr <- ExprGuiM.makeSubexpression 0 rest <&> pad config
    let minWidth = restExpr ^. ExpressionGui.egWidget . Widget.wSize . Lens._1
    return $ ExpressionGui.vboxDownwards $
      (,) 0.5 <$>
      [ fieldsGui
      , Anim.unitSquare (Widget.toAnimId (Widget.joinId myId ["tail"]))
        & Anim.onImages (Draw.tint (Config.recordTailColor config))
        & Widget.liftView 1
        & Widget.scale (Vector2 (max minWidth targetWidth) 10)
        & ExpressionGui.fromValueWidget
      , ExpressionGui.fromValueWidget vspace
      , restExpr
      ]
  where
    targetWidth = fieldsGui ^. ExpressionGui.egWidget . Widget.wSize . Lens._1

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
            makeOpenRecord fieldsGui rest myId
    let
      eventMap =
        maybe mempty
        (Widget.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
         (E.Doc ["Edit", "Record", "Add Field"]) .
         fmap (TagEdit.diveIntoRecordTag . WidgetIds.fromEntityId) .
         (ExprGuiM.holePickersAction resultPickers >>)) mAddField
    gui
      & ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
      & ExpressionGui.egWidget %%~ ExpressionGui.addValBG myId
  where
    defaultPos =
      case fields of
      [] -> myId
      (f : _) ->
        f ^. Sugar.rfExpr . Sugar.rPayload . Sugar.plEntityId
        & WidgetIds.fromEntityId

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
