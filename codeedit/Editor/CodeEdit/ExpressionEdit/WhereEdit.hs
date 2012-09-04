{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.WhereEdit(make, makeWithBody) where

import Control.Monad (liftM)
import Data.Function (on)
import Data.Monoid (mempty)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess, WidgetT)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
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
  -> VarAccess m (WidgetT m)
make makeExpressionEdit (Sugar.Where items _) myId = do
  whereLabel <-
    VarAccess.atEnv (OT.setTextSizeColor Config.whereTextSize Config.whereColor) .
    VarAccess.otransaction $ BWidgets.makeLabel "where" $ Widget.toAnimId myId
  whereEdits <- mapM makeWhereItemEdits items
  return $ BWidgets.vboxCentered
    [ whereLabel
    , Widget.scale Config.whereScaleFactor . Grid.toWidget $
      Grid.makeAlign 0 whereEdits
    ]
  where
    makeWhereItemEdits item =
      on VarAccess.assignCursorPrefix WidgetIds.fromGuid
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
  -> Sugar.HasParens
  -> Sugar.Where m
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
makeWithBody makeExpressionEdit hasParens where_@(Sugar.Where _ body) myId = do
  whereEdit <- make makeExpressionEdit where_ myId
  bodyEdit <-
    VarAccess.assignCursor myId ((WidgetIds.fromGuid . Sugar.rGuid) body) $
    makeExpressionEdit body
  let
    res =
      ExpressionGui.fromValueWidget $ BWidgets.vboxCentered
      [ ExpressionGui.egWidget bodyEdit
      , whereEdit
      ]
  return $ case hasParens of
    Sugar.DontHaveParens -> res
    Sugar.HaveParens ->
      ExpressionGui.atEgWidget
      (Parens.addSquareParens (Widget.toAnimId myId))
      res
