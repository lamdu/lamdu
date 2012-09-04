{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Section m
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make makeExpressionEdit (Sugar.Section mLArg op mRArg innerApplyGuid) myId =
  VarAccess.assignCursor myId destId .
    maybe id ((`VarAccess.assignCursor` destId) . WidgetIds.fromGuid) innerApplyGuid $ do
      lArgEdits <- fromMArg mLArg
      opEdits <- makeExpressionsEdit op
      rArgEdits <- fromMArg mRArg
      return . ExpressionGui.hboxSpaced $ lArgEdits ++ opEdits ++ rArgEdits
  where
    destId = WidgetIds.fromGuid . Sugar.rGuid $ fromMaybe op mRArg
    makeExpressionsEdit = liftM (:[]) . makeExpressionEdit
    fromMArg = maybe (return []) makeExpressionsEdit
