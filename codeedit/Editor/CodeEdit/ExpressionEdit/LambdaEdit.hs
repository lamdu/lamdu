{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.LambdaEdit(make) where

import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Data as Data
import qualified Graphics.UI.Bottle.Widget as Widget

lambdaToFunc :: MonadF m => IRef Data.Expression -> Data.Lambda -> Sugar.Func m
lambdaToFunc expressionI (Data.Lambda paramI bodyI) =
  Sugar.Func [paramI] bodyIPtr
  where
    expressionRef = Transaction.fromIRef expressionI
    bodyIPtr =
      Property (return bodyI) $ Property.set expressionRef . Data.ExpressionLambda . (paramI `Data.Lambda`)

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Data.Lambda
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr lambda myId = do
  expressionI <- getP expressionPtr
  FuncEdit.make makeExpressionEdit ancestry expressionPtr (lambdaToFunc expressionI lambda) myId
