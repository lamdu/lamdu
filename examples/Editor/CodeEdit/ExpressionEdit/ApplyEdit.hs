{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor, transaction)
import Editor.CodeEdit.Types(ApplyData(..))
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make ::
  (MonadF m) =>
  (ETypes.ExpressionAncestry m
   -> ETypes.ExpressionPtr m
   -> CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id))
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Data.Apply
  -> Widget.Id
  -> CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
make makeExpressionEdit ancestry expressionPtr apply@(Data.Apply funcI argI) myId = do
  expressionI <- getP expressionPtr
  assignCursor myId (WidgetIds.fromIRef argI) $ do
    isInfix <- transaction $ ETypes.isInfixFunc funcI
    isApplyOfInfix <- transaction $ ETypes.isApplyOfInfixOp funcI
    let
      funcType
        | isInfix = ETypes.InfixLeft
        | isApplyOfInfix = ETypes.InfixRight
        | otherwise = ETypes.Prefix
      expressionRef = Transaction.fromIRef expressionI
      delEventMap = Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
      funcIPtr = Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
      argIPtr = Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
      setExpr newExprI = do
        Property.set expressionPtr newExprI
        return $ WidgetIds.fromIRef newExprI

      addDelEventMap =
        liftM . first . Widget.weakerEvents . delEventMap

    let
      makeAncestry role =
        ApplyData {
          adRole = role,
          adFuncType = funcType,
          adApply = apply,
          adParentPtr = expressionPtr
          }
        : ancestry
    (funcEdit, parenId) <-
      addDelEventMap argI $
      makeExpressionEdit (makeAncestry ETypes.ApplyFunc) funcIPtr

    (argEdit, _) <-
      addDelEventMap funcI $
      makeExpressionEdit (makeAncestry ETypes.ApplyArg) argIPtr

    return
      ((BWidgets.hbox . if isInfix then reverse else id)
       [funcEdit, BWidgets.spaceWidget, argEdit], parenId)
