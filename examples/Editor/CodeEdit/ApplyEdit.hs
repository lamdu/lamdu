{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ApplyEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Monoid(Monoid(..))
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, assignCursor)
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
  -> ETypes.ExpressionPtr m
  -> Data.Apply
  -> Widget.Id
  -> CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
make makeExpressionEdit expressionPtr apply@(Data.Apply funcI argI) myId = do
  expressionI <- getP expressionPtr
  assignCursor myId (WidgetIds.fromIRef argI) $ do
    isInfix <- ETypes.isInfixFunc funcI
    let
      funcType
        | isInfix = ETypes.Infix
        | otherwise = ETypes.Prefix
      expressionRef = Transaction.fromIRef expressionI
      delEventMap = Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
      funcIPtr = Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
      argIPtr = Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
      setExpr newExprI = do
        Property.set expressionPtr newExprI
        return $ WidgetIds.fromIRef newExprI
      addNextArgEventMap = ETypes.makeAddNextArgEventMap expressionPtr
      funcEvents =
        Widget.weakerEvents (delEventMap argI) .
        if isInfix
        then Widget.strongerEvents addNextArgEventMap
        else id
    (funcEdit, parenId) <-
      (liftM . first) funcEvents $ makeExpressionEdit ETypes.NotArgument funcIPtr
    (argEdit, _) <-
       (liftM . first . Widget.weakerEvents . mconcat)
       [ addNextArgEventMap
       , delEventMap funcI
       ] $ makeExpressionEdit (ETypes.Argument (ETypes.ArgumentData funcType expressionPtr apply)) argIPtr
    return
      ((BWidgets.hbox . if isInfix then reverse else id)
       [funcEdit, BWidgets.spaceWidget, argEdit], parenId)
