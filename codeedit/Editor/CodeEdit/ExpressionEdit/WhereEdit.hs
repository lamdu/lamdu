{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.WhereEdit(make, makeWithBody) where

import Control.Monad (liftM, (<=<))
import Data.Monoid (mempty)
import Editor.Anchors (ViewTag)
import Editor.MonadF (MonadF)
import Editor.OTransaction (TWidget)
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
  -> Widget.Id -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Where items _) myId = do
  whereLabel <-
    OT.setTextSizeColor Config.whereTextSize Config.whereColor $
    BWidgets.makeLabel "where" $ Widget.toAnimId myId
  let
    makeWhereItemsGrid =
      liftM (Grid.toWidget . Grid.makeAlign 0 . concat) $
      mapM makeWhereItemEdits items
  whereEdits <- makeWhereItemsGrid
  return . BWidgets.vboxCentered $
    [ whereLabel
    , Widget.scale Config.whereScaleFactor whereEdits
    ]
  where
    makeWhereItemEdits item =
      (liftM . map . map . Widget.weakerEvents)
        (whereItemDeleteEventMap item) $
      DefinitionEdit.makeParts makeExpressionEdit
      (paramId item) (guid item) (Sugar.wiValue item) (Sugar.wiType item)
    paramId = WidgetIds.paramId . guid
    guid = Sugar.guid . Sugar.wiEntity
    whereItemDeleteEventMap whereItem =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.delKeys "Delete variable" .
       liftM WidgetIds.fromGuid . IT.transaction)
      ((Sugar.mDelete <=< Sugar.eActions . Sugar.wiEntity) whereItem)

makeWithBody
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
makeWithBody makeExpressionEdit where_@(Sugar.Where _ body) myId = do
  whereEdit <- make makeExpressionEdit where_ myId
  OT.assignCursor myId ((WidgetIds.fromGuid . Sugar.guid . Sugar.rEntity) body) $ do
    bodyEdit <- makeExpressionEdit body
    return . BWidgets.vboxCentered $
      [ bodyEdit
      , whereEdit
      ]
