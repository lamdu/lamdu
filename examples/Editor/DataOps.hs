{-# OPTIONS -Wall #-}
module Editor.DataOps (addParameter, delParameter, giveAsArg, callWithArg) where

import Data.List (delete)
import Data.Store.IRef (IRef)
import Data.Store.Property(Property)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

addParameter ::
  Monad m => Transaction.Property t m Data.Definition ->
  Transaction t m (IRef Data.Parameter)
addParameter definitionRef = do
  newParamI <- Transaction.newIRef Data.Parameter
  Property.pureModify definitionRef . Data.atDefParameters $
    (++ [newParamI])
  return newParamI

delParameter ::
  Monad m => Transaction.Property t m Data.Definition ->
  IRef Data.Parameter -> Transaction t m ()
delParameter definitionRef paramI =
  Property.pureModify definitionRef . Data.atDefParameters $
    delete paramI

giveAsArg ::
  Monad m =>
  Property (Transaction t m) (IRef Data.Expression) ->
  Transaction t m (IRef Data.Expression)
giveAsArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  newFuncI <- Transaction.newIRef . Data.ExpressionGetVariable . Data.GetVariable =<< Transaction.newIRef ""
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply newFuncI expressionI)
  return newFuncI

callWithArg ::
  Monad m =>
  Property (Transaction t m) (IRef Data.Expression) ->
  Transaction t m (IRef Data.Expression)
callWithArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  argI <- Transaction.newIRef . Data.ExpressionGetVariable . Data.GetVariable =<< Transaction.newIRef ""
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply expressionI argI)
  return argI
