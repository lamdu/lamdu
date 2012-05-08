{-# OPTIONS -Wall #-}
module Editor.DataOps (
  ExpressionPtr,
  newHole, giveAsArg, callWithArg,
  replace, replaceWithHole,
  lambdaWrap, lambdaBodyRef, lambdaParamTypeRef,
  applyFuncRef, applyArgRef)
where

import Data.Store.IRef (IRef)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
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
newHole = Transaction.newIRef Data.ExpressionHole

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
  -> Transaction ViewTag m (IRef Data.Expression)
lambdaWrap expressionPtr = do
  newParamTypeI <- newHole
  expressionI <- Property.get expressionPtr
  newExpressionI <-
    Transaction.newIRef . Data.ExpressionLambda $
    Data.Lambda newParamTypeI expressionI
  Property.set expressionPtr newExpressionI
  return newExpressionI

lambdaBodyRef :: Monad m => IRef Data.Expression -> Data.Lambda -> ExpressionPtr m
lambdaBodyRef lambdaI (Data.Lambda paramTypeI bodyI) =
  Property
    (return bodyI)
    (Transaction.writeIRef lambdaI . Data.ExpressionLambda . Data.Lambda paramTypeI)

lambdaParamTypeRef :: Monad m => IRef Data.Expression -> Data.Lambda -> ExpressionPtr m
lambdaParamTypeRef lambdaI (Data.Lambda paramTypeI bodyI) =
  Property
    (return paramTypeI)
    (Transaction.writeIRef lambdaI . Data.ExpressionLambda . buildLambda)
  where
    buildLambda newTypeI = Data.Lambda newTypeI bodyI

applyFuncRef :: Monad m => IRef Data.Expression -> Data.Apply -> ExpressionPtr m
applyFuncRef applyI (Data.Apply funcI argI) =
  Property (return funcI) $
  Transaction.writeIRef applyI .
  Data.ExpressionApply . (`Data.Apply` argI)

applyArgRef :: Monad m => IRef Data.Expression -> Data.Apply -> ExpressionPtr m
applyArgRef applyI (Data.Apply funcI argI) =
  Property (return argI) $
  Transaction.writeIRef applyI .
  Data.ExpressionApply . Data.Apply funcI
