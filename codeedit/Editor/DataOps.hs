{-# LANGUAGE DeriveFunctor #-}
module Editor.DataOps
  ( ExpressionSetter
  , newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap
  , lambdaBodySetter, lambdaParamTypeSetter
  , applyFuncSetter, applyArgSetter
  )
where

import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

type ExpressionSetter m = IRef Data.Expression -> Transaction ViewTag m ()

giveAsArg ::
  Monad m =>
  IRef Data.Expression ->
  ExpressionSetter m ->
  Transaction ViewTag m (IRef Data.Expression)
giveAsArg exprI setExprI = do
  newFuncI <- newHole
  setExprI =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply newFuncI exprI)
  return newFuncI

callWithArg ::
  Monad m =>
  IRef Data.Expression -> ExpressionSetter m ->
  Transaction ViewTag m (IRef Data.Expression)
callWithArg exprI setExprI = do
  argI <- newHole
  setExprI =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply exprI argI)
  return argI

newHole :: Monad m => Transaction ViewTag m (IRef Data.Expression)
newHole = Transaction.newIRef Data.ExpressionHole

replace
  :: Monad m
  => ExpressionSetter m
  -> IRef Data.Expression
  -> Transaction ViewTag m (IRef Data.Expression)
replace setExprI newExprI = do
  setExprI newExprI
  return newExprI

replaceWithHole
  :: Monad m
  => ExpressionSetter m
  -> Transaction ViewTag m (IRef Data.Expression)
replaceWithHole setExprI = replace setExprI =<< newHole

lambdaWrap
  :: Monad m
  => IRef Data.Expression -> ExpressionSetter m
  -> Transaction ViewTag m (IRef Data.Expression)
lambdaWrap exprI setExprI = do
  newParamTypeI <- newHole
  newExprI <-
    Transaction.newIRef . Data.ExpressionLambda $
    Data.Lambda newParamTypeI exprI
  setExprI newExprI
  return newExprI

lambdaBodySetter :: Monad m => IRef Data.Expression -> Data.Lambda -> ExpressionSetter m
lambdaBodySetter lambdaI (Data.Lambda paramTypeI _) =
  Transaction.writeIRef lambdaI . Data.ExpressionLambda . Data.Lambda paramTypeI

lambdaParamTypeSetter :: Monad m => IRef Data.Expression -> Data.Lambda -> ExpressionSetter m
lambdaParamTypeSetter lambdaI (Data.Lambda _ bodyI) =
  Transaction.writeIRef lambdaI . Data.ExpressionLambda . (`Data.Lambda` bodyI)

applyFuncSetter :: Monad m => IRef Data.Expression -> Data.Apply -> ExpressionSetter m
applyFuncSetter applyI (Data.Apply _ argI) =
  Transaction.writeIRef applyI . Data.ExpressionApply . (`Data.Apply` argI)

applyArgSetter :: Monad m => IRef Data.Expression -> Data.Apply -> ExpressionSetter m
applyArgSetter applyI (Data.Apply funcI _) =
  Transaction.writeIRef applyI . Data.ExpressionApply . Data.Apply funcI
