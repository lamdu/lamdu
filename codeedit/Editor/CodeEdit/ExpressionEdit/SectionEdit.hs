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
import qualified Editor.CodeEdit.Parens as Parens

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.HasParens
  -> Sugar.Section m
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make makeExpressionEdit hasParens (Sugar.Section mLArg op mRArg innerApplyGuid) myId =
  VarAccess.assignCursor myId destId .
    maybe id ((`VarAccess.assignCursor` destId) . WidgetIds.fromGuid) innerApplyGuid $ do
      lArgEdits <- fromMArg mLArg
      opEdits <- makeExpressionsEdit op
      rArgEdits <- fromMArg mRArg
      ExpressionGui.parenify hasParens parenAdder myId .
        ExpressionGui.hboxSpaced $ lArgEdits ++ opEdits ++ rArgEdits
  where
    parenAdder = case (mLArg, mRArg) of
      (Nothing, Nothing) -> Parens.addTextParens . Widget.toAnimId
      _ -> Parens.addHighlightedTextParens
    destId = WidgetIds.fromGuid . Sugar.rGuid $ fromMaybe op mRArg
    makeExpressionsEdit = liftM (:[]) . makeExpressionEdit
    fromMArg = maybe (return []) makeExpressionsEdit
