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
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
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
  Sugar.Record Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make reco pl = ExpressionGui.stdWrapParentExpr pl $ makeUnwrapped reco

makeUnwrapped ::
  MonadA m =>
  Sugar.Record Sugar.Name m (ExprGuiM.SugarExpr m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.Record fields _tail mAddField) myId =
  ExprGuiM.assignCursor myId tailId $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      makeFieldRow (Sugar.RecordField mDelete tag fieldExpr) = do
        fieldRefGui <- TagEdit.make tag (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
        fieldExprGui <- ExprGuiM.makeSubexpression 0 fieldExpr
        let
          itemEventMap = maybe mempty (recordItemEventMap config) mDelete
          space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget
        return . ExpressionGui.makeRow $
          [(1, scaleTag fieldRefGui), (0.5, space), (0, fieldExprGui)]
          & Lens.mapped . Lens._2 . ExpressionGui.egWidget %~
            Widget.weakerEvents itemEventMap
      scaleTag =
        ExpressionGui.egWidget %~
        Widget.scale (realToFrac <$> Config.fieldTagScaleFactor config)
    fieldRows <- mapM makeFieldRow fields
    let
      fieldsWidget = Grid.toWidget $ Grid.make fieldRows
      targetWidth = max 250 $ fieldsWidget ^. Widget.wSize . Lens._1
    tailWidget <-
      Anim.unitSquare (Widget.toAnimId tailId)
      & Anim.onImages (Draw.tint (Config.recordTailColor config))
      & Widget.liftView 1
      & Widget.scale (Vector2 targetWidth 10)
      & ExprGuiM.widgetEnv . BWidgets.makeFocusableView tailId
    let
      eventMap =
        mkEventMap (fmap WidgetIds.fromEntityId)
        mAddField (Config.recordAddFieldKeys config) $
        E.Doc ["Edit", "Record", "Add First Field"]
    return . ExpressionGui.fromValueWidget . Widget.weakerEvents eventMap $
      Box.vboxCentered
      [ fieldsWidget
      , tailWidget
      ]
  where
    tailId = Widget.joinId myId ["tail"]
    mkEventMap f mAction keys doc =
      maybe mempty (Widget.keysEventMapMovesCursor keys doc . f) mAction

recordItemEventMap ::
  MonadA m => Config ->
  T m Sugar.EntityId -> Widget.EventHandlers (T m)
recordItemEventMap config delete =
  Widget.keysEventMapMovesCursor (Config.delKeys config)
  (E.Doc ["Edit", "Record", "Delete Field"]) $ WidgetIds.fromEntityId <$> delete
