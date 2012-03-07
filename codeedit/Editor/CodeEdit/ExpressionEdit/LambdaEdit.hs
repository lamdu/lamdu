{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.LambdaEdit(make) where

import Editor.CodeEdit.Types(AncestryItem(..), LambdaParent(..))
import Data.Store.Property (Property(Property))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => (ETypes.ExpressionAncestry m
   -> ETypes.ExpressionPtr m
   -> TWidget ViewTag m)
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Data.Lambda -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr lambda myId =
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    expressionI <- getP expressionPtr
    paramEdit <- ParamEdit.make paramI
    lambdaLabel <- BWidgets.makeLabel "λ" myId
    rightArrowLabel <- BWidgets.makeLabel "→" myId
    let
      expressionRef = Transaction.fromIRef expressionI
      bodyIPtr =
        Property (return bodyI) $ Property.set expressionRef . Data.ExpressionLambda . (paramI `Data.Lambda`)
      ancestryItem = AncestryItemLambda $ LambdaParent lambda expressionPtr
    bodyEdit <- makeExpressionEdit (ancestryItem : ancestry) bodyIPtr
    return $ BWidgets.hbox [
      lambdaLabel,
      paramEdit,
      rightArrowLabel,
      bodyEdit
      ]
  where
    paramI = Data.lambdaParam lambda
    bodyI = Data.lambdaBody lambda
