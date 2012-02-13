{-# OPTIONS -Wall #-}
module Editor.DataOps (
  newHole, addParameter, delParameter, giveAsArg, callWithArg, replace,
  addAsParameter, addAsDefinition)
where

import Data.List (delete)
import Data.Store.IRef (IRef)
import Data.Store.Property(Property)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
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
  newFuncI <- newHole
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply newFuncI expressionI)
  return newFuncI

callWithArg ::
  Monad m =>
  Property (Transaction t m) (IRef Data.Expression) ->
  Transaction t m (IRef Data.Expression)
callWithArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  argI <- newHole
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply expressionI argI)
  return argI

newHole :: Monad m => Transaction t m (IRef Data.Expression)
newHole =
  Transaction.newIRef . Data.ExpressionHole $ Data.HoleState
  { Data.holeSearchTerm = ""
  --, Data.holeCachedSearchResults = []
  }

replace ::
  Monad m =>
  Property (Transaction t m) (IRef Data.Expression) ->
  Transaction t m (IRef Data.Expression)
replace expressionPtr = do
  exprI <- newHole
  Property.set expressionPtr exprI
  return exprI

addAsParameter ::
  Monad m => String -> Transaction.Property t m Data.Definition ->
  IRef Data.Expression -> Transaction t m ()
addAsParameter newName definitionRef expressionI = do
  newParam <- addParameter definitionRef
  Property.set (Anchors.aNameRef newParam) newName
  Transaction.writeIRef expressionI . Data.ExpressionGetVariable $ Data.ParameterRef newParam

addAsDefinition ::
  Monad m => String -> IRef Data.Expression ->
  Transaction Anchors.ViewTag m (IRef Data.Definition)
addAsDefinition newName expressionI = do
  newDefI <- Anchors.makeDefinition
  Property.set (Anchors.aNameRef newDefI) newName
  Transaction.writeIRef expressionI . Data.ExpressionGetVariable $ Data.DefinitionRef newDefI
  Anchors.newPane newDefI
  return newDefI
