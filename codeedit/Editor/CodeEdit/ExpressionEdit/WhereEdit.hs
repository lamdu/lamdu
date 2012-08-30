{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.WhereEdit(make, makeWithBody) where

import Control.Monad (liftM)
import Data.Function (on)
import Data.Monoid (mempty)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, WidgetT)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Where m
  -> Widget.Id
  -> OTransaction ViewTag m (WidgetT ViewTag m)
make makeExpressionEdit (Sugar.Where items _) myId = do
  whereLabel <-
    OT.setTextSizeColor Config.whereTextSize Config.whereColor $
    BWidgets.makeLabel "where" $ Widget.toAnimId myId
  let
    makeWhereItemsGrid =
      liftM (Grid.toWidget . Grid.makeAlign 0) $
      mapM makeWhereItemEdits items
  whereEdits <- makeWhereItemsGrid
  return . BWidgets.vboxCentered $
    [ whereLabel
    , Widget.scale Config.whereScaleFactor whereEdits
    ]
  where
    makeWhereItemEdits item =
      on OT.assignCursorPrefix WidgetIds.fromGuid
      (Sugar.wiTypeGuid item) (Sugar.wiGuid item) .
      (liftM . map)
        (Widget.weakerEvents (whereItemDeleteEventMap item) . ExpressionGui.egWidget) $
      DefinitionEdit.makeParts makeExpressionEdit
      (Sugar.wiGuid item) (Sugar.wiValue item)
    whereItemDeleteEventMap whereItem =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.delKeys "Delete variable" .
       liftM WidgetIds.fromGuid . IT.transaction)
      (Sugar.wiMDelete whereItem)

makeWithBody
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Where m
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
makeWithBody makeExpressionEdit where_@(Sugar.Where _ body) myId = do
  whereEdit <- make makeExpressionEdit where_ myId
  OT.assignCursor myId ((WidgetIds.fromGuid . Sugar.rGuid) body) $ do
    bodyEdit <- makeExpressionEdit body
    return . ExpressionGui.fromValueWidget . BWidgets.vboxCentered $
      [ ExpressionGui.egWidget bodyEdit
      , whereEdit
      ]
