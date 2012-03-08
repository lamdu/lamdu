{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.LambdaEdit(make) where

import Control.Monad(liftM)
import Data.Store.Property (Property(Property))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, atTextSizeColor)
import Editor.CodeEdit.Types(AncestryItem(..), LambdaParent(..))
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
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
    lambdaLabel <-
      atTextSizeColor Config.lambdaTextSize Config.lambdaColor $
      BWidgets.makeLabel "λ" myId
    rightArrowLabel <-
      atTextSizeColor Config.rightArrowTextSize Config.rightArrowColor $
      BWidgets.makeLabel "→" myId
    let
      expressionRef = Transaction.fromIRef expressionI
      bodyIPtr =
        Property (return bodyI) $ Property.set expressionRef . Data.ExpressionLambda . (paramI `Data.Lambda`)
      ancestryItem = AncestryItemLambda $ LambdaParent lambda expressionPtr
      paramEditEventMap =
        Widget.actionEventMapMovesCursor Config.delKeys "Remove lambda wrapper" $
        setExpr bodyI
    bodyEdit <- makeExpressionEdit (ancestryItem : ancestry) bodyIPtr
    return $ BWidgets.hbox [
      lambdaLabel,
      Widget.weakerEvents paramEditEventMap paramEdit,
      rightArrowLabel,
      bodyEdit
      ]
  where
    setExpr = liftM WidgetIds.fromIRef . DataOps.replace expressionPtr
    paramI = Data.lambdaParam lambda
    bodyI = Data.lambdaBody lambda
