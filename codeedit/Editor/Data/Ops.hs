{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
  ( ExpressionSetter
  , newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  , lambdaTypeSetter, lambdaBodySetter
  , applyFuncSetter, applyArgSetter
  )
where

import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

type ExpressionSetter m =
  IRef (Data.Expression IRef) -> Transaction ViewTag m ()

giveAsArg ::
  Monad m =>
  IRef (Data.Expression IRef) ->
  ExpressionSetter m ->
  Transaction ViewTag m (IRef (Data.Expression IRef))
giveAsArg exprI setExprI = do
  newFuncI <- newHole
  setExprI =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply newFuncI exprI)
  return newFuncI

callWithArg ::
  Monad m =>
  IRef (Data.Expression IRef) -> ExpressionSetter m ->
  Transaction ViewTag m (IRef (Data.Expression IRef))
callWithArg exprI setExprI = do
  argI <- newHole
  setExprI =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply exprI argI)
  return argI

newHole :: Monad m => Transaction ViewTag m (IRef (Data.Expression IRef))
newHole = Transaction.newIRef Data.ExpressionHole

replace
  :: Monad m
  => ExpressionSetter m
  -> IRef (Data.Expression IRef)
  -> Transaction ViewTag m (IRef (Data.Expression IRef))
replace setExprI newExprI = do
  setExprI newExprI
  return newExprI

replaceWithHole
  :: Monad m
  => ExpressionSetter m
  -> Transaction ViewTag m (IRef (Data.Expression IRef))
replaceWithHole setExprI = replace setExprI =<< newHole

lambdaWrap
  :: Monad m
  => IRef (Data.Expression IRef) -> ExpressionSetter m
  -> Transaction ViewTag m (IRef (Data.Expression IRef))
lambdaWrap exprI setExprI = do
  newParamTypeI <- newHole
  newExprI <-
    Transaction.newIRef . Data.ExpressionLambda $
    Data.Lambda newParamTypeI exprI
  setExprI newExprI
  return newExprI

redexWrap
  :: Monad m
  => IRef (Data.Expression IRef) -> ExpressionSetter m
  -> Transaction ViewTag m (IRef (Data.Expression IRef))
redexWrap exprI setExprI = do
  newParamTypeI <- newHole
  newLambdaI <-
    Transaction.newIRef . Data.ExpressionLambda $
    Data.Lambda newParamTypeI exprI
  newValueI <- newHole
  newApplyI <-
    Transaction.newIRef . Data.ExpressionApply $
    Data.Apply newLambdaI newValueI
  setExprI newApplyI
  return newLambdaI

lambdaTypeSetter
  :: Monad m
  => (Data.Lambda IRef -> Data.Expression IRef) -> IRef (Data.Expression IRef) -> Data.Lambda IRef -> ExpressionSetter m
lambdaTypeSetter cons lambdaI (Data.Lambda _ bodyI) =
  Transaction.writeIRef lambdaI . cons . flip Data.Lambda bodyI

lambdaBodySetter
  :: Monad m
  => (Data.Lambda IRef -> Data.Expression IRef) -> IRef (Data.Expression IRef) -> Data.Lambda IRef -> ExpressionSetter m
lambdaBodySetter cons lambdaI (Data.Lambda paramTypeI _) =
  Transaction.writeIRef lambdaI . cons . Data.Lambda paramTypeI

applyFuncSetter :: Monad m => IRef (Data.Expression IRef) -> Data.Apply IRef -> ExpressionSetter m
applyFuncSetter applyI (Data.Apply _ argI) =
  Transaction.writeIRef applyI . Data.ExpressionApply . (`Data.Apply` argI)

applyArgSetter :: Monad m => IRef (Data.Expression IRef) -> Data.Apply IRef -> ExpressionSetter m
applyArgSetter applyI (Data.Apply funcI _) =
  Transaction.writeIRef applyI . Data.ExpressionApply . Data.Apply funcI
