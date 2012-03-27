{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.LambdaEdit(make) where

import Data.Store.Property (Property(Property))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, getP, transaction)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Data as Data
import qualified Graphics.UI.Bottle.Widget as Widget

lambdaToFunc :: MonadF m => ETypes.ExpressionPtr m -> Data.Lambda -> CTransaction ViewTag m (Sugar.Func m)
lambdaToFunc exprPtr lambda@(Data.Lambda paramI bodyI) = do
  expressionI <- getP exprPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    bodyIPtr =
      Property (return bodyI) $ Property.set expressionRef . Data.ExpressionLambda . (paramI `Data.Lambda`)
  func <- transaction $ Sugar.funcParamOfLambda exprPtr lambda
  return $ Sugar.Func [func] bodyIPtr

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Data.Lambda
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr lambda myId = do
  func <- lambdaToFunc expressionPtr lambda
  FuncEdit.make makeExpressionEdit ancestry expressionPtr func myId
