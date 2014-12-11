{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit(make) where

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
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
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

diveIntoTagEdit :: Widget.Id -> Widget.Id
diveIntoTagEdit = FocusDelegator.delegatingId

makeUnwrapped ::
  MonadA m =>
  Sugar.Record (Name m) m (ExprGuiM.SugarExpr m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.Record fields recordTail mAddField) myId =
  ExprGuiM.assignCursor myId defaultPos $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      vspace =
        BWidgets.vspaceWidget . realToFrac $
        Config.spaceBetweenAnnotatedArgs config
      pad = Widget.pad $ realToFrac <$> Config.valFramePadding config
      makeFieldRow (Sugar.RecordField mDelete tag fieldExpr) = do
        fieldRefGui <-
          TagEdit.make (ExprGuiM.nextHolesBefore fieldExpr)
          tag (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
        fieldExprGui <- ExprGuiM.makeSubexpression 0 fieldExpr
        let
          itemEventMap = maybe mempty (recordDelEventMap config) mDelete
          space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget
        [(1, scaleTag fieldRefGui), (0.5, space), (0, fieldExprGui)]
          <&> Lens._2 . ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
          & ExpressionGui.makeRow
          & return
      scaleTag =
        ExpressionGui.egWidget %~
        Widget.scale (realToFrac <$> Config.fieldTagScaleFactor config)
    (widget, resultPickers) <-
      ExprGuiM.listenResultPickers $ do
        fieldRows <-
          mapM makeFieldRow fields
          <&> List.intersperse (replicate 3 (0.5, vspace))
        fieldsWidget <-
          if null fieldRows
          then
            BWidgets.grammarLabel "Ø" myId
            >>= BWidgets.makeFocusableView myId
            & ExprGuiM.widgetEnv
          else Grid.make fieldRows & Grid.toWidget & pad & return
        let
          targetWidth =
            case fieldRows of
            [] -> 250
            _ -> fieldsWidget ^. Widget.wSize . Lens._1
        case recordTail of
          Sugar.ClosedRecord mDeleteTail ->
            fieldsWidget
            & Widget.weakerEvents
              (maybe mempty (recordOpenEventMap config) mDeleteTail)
            & return
          Sugar.RecordExtending rest -> do
            restExpr <-
              ExprGuiM.makeSubexpression 0 rest
              <&> (^. ExpressionGui.egWidget)
              <&> pad
            let minWidth = restExpr ^. Widget.wSize . Lens._1
            return $ Box.vboxCentered
              [ fieldsWidget
              , Anim.unitSquare (Widget.toAnimId (Widget.joinId myId ["tail"]))
                & Anim.onImages (Draw.tint (Config.recordTailColor config))
                & Widget.liftView 1
                & Widget.scale (Vector2 (max minWidth targetWidth) 10)
              , vspace
              , restExpr
              ]
    let
      eventMap =
        mkEventMap (fmap (diveIntoTagEdit . WidgetIds.fromEntityId))
        ((ExprGuiM.holePickersAction resultPickers >>) <$> mAddField)
        (Config.recordAddFieldKeys config) $
        E.Doc ["Edit", "Record", "Add Field"]
    Widget.weakerEvents eventMap widget
      & ExpressionGui.fromValueWidget
      & ExpressionGui.withBgColor (Config.layerValFrameBG (Config.layers config))
        (Config.valFrameBGColor config) (Widget.toAnimId myId ++ ["bg"])
      & return
  where
    defaultPos =
      case fields of
      [] -> myId
      (f : _) ->
        f ^. Sugar.rfExpr . Sugar.rPayload . Sugar.plEntityId
        & WidgetIds.fromEntityId
    mkEventMap f mAction keys doc =
      maybe mempty (Widget.keysEventMapMovesCursor keys doc . f) mAction

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
