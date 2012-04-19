{-# OPTIONS -Wall #-}
module Editor.DataOps (
  ExpressionPtr,
  newHole, giveAsArg, callWithArg,
  replace, replaceWithHole,
  addAsDefinition, lambdaWrap, lambdaBodyRef, lambdaParamTypeRef)
where

import Data.Store.IRef (IRef)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data

type ExpressionPtr m = Transaction.Property ViewTag m (IRef Data.Expression)

giveAsArg ::
  Monad m =>
  ExpressionPtr m ->
  Transaction ViewTag m (IRef Data.Expression)
giveAsArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  newFuncI <- newHole
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply newFuncI expressionI)
  return newFuncI

callWithArg ::
  Monad m =>
  ExpressionPtr m ->
  Transaction ViewTag m (IRef Data.Expression)
callWithArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  argI <- newHole
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply expressionI argI)
  return argI

newHole :: Monad m => Transaction ViewTag m (IRef Data.Expression)
newHole =
  Transaction.newIRef . Data.ExpressionHole $ Data.HoleState
  { Data.holeSearchTerm = ""
  --, Data.holeCachedSearchResults = []
  }

replace
  :: Monad m
  => ExpressionPtr m
  -> IRef Data.Expression
  -> Transaction ViewTag m (IRef Data.Expression)
replace expressionPtr newExpressionI = do
  Property.set expressionPtr newExpressionI
  return newExpressionI

replaceWithHole
  :: Monad m
  => ExpressionPtr m
  -> Transaction ViewTag m (IRef Data.Expression)
replaceWithHole expressionPtr = replace expressionPtr =<< newHole

lambdaWrap
  :: Monad m
  => ExpressionPtr m
  -> Transaction ViewTag m (IRef Data.Parameter)
lambdaWrap expressionPtr = do
  newParamI <- Transaction.newIRef Data.Parameter
  newParamTypeI <- newHole
  expressionI <- Property.get expressionPtr
  let newLambda = Data.Lambda (Data.TypedParam newParamI newParamTypeI) expressionI
  newExpressionI <- Transaction.newIRef $ Data.ExpressionLambda newLambda
  Property.set expressionPtr newExpressionI
  return newParamI

addAsDefinition ::
  Monad m => String -> IRef Data.Expression ->
  Transaction ViewTag m (IRef Data.Definition)
addAsDefinition newName expressionI = do
  newDefI <- Anchors.makeDefinition
  Property.set (Anchors.aNameRef newDefI) newName
  Transaction.writeIRef expressionI . Data.ExpressionGetVariable $ Data.DefinitionRef newDefI
  Anchors.newPane newDefI
  return newDefI

lambdaBodyRef :: Monad m => IRef Data.Expression -> Data.Lambda -> ExpressionPtr m
lambdaBodyRef lambdaIRef (Data.Lambda (Data.TypedParam paramI paramTypeI) bodyI) =
  Property
    (return bodyI)
    (Transaction.writeIRef lambdaIRef . Data.ExpressionLambda . Data.Lambda (Data.TypedParam paramI paramTypeI))

lambdaParamTypeRef :: Monad m => IRef Data.Expression -> Data.Lambda -> ExpressionPtr m
lambdaParamTypeRef lambdaIRef (Data.Lambda (Data.TypedParam paramI paramTypeI) bodyI) =
  Property
    (return paramTypeI)
    (Transaction.writeIRef lambdaIRef . Data.ExpressionLambda . buildLambda)
  where
    buildLambda newTypeI = Data.Lambda (Data.TypedParam paramI newTypeI) bodyI
