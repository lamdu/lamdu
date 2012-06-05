{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
  ( ExpressionSetter
  , newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  , lambdaTypeSetter, lambdaBodySetter
  , applyFuncSetter, applyArgSetter
  )
where

import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import qualified Editor.Data as Data

type ExpressionSetter m = Data.ExpressionIRef -> Transaction ViewTag m ()

giveAsArg ::
  Monad m =>
  Data.ExpressionIRef ->
  ExpressionSetter m ->
  Transaction ViewTag m Data.ExpressionIRef
giveAsArg exprI setExprI = do
  newFuncI <- newHole
  setExprI =<< (Data.newExprIRef . Data.ExpressionApply) (Data.Apply newFuncI exprI)
  return newFuncI

callWithArg ::
  Monad m =>
  Data.ExpressionIRef -> ExpressionSetter m ->
  Transaction ViewTag m Data.ExpressionIRef
callWithArg exprI setExprI = do
  argI <- newHole
  setExprI =<< (Data.newExprIRef . Data.ExpressionApply) (Data.Apply exprI argI)
  return argI

newHole :: Monad m => Transaction ViewTag m Data.ExpressionIRef
newHole = Data.newExprIRef Data.ExpressionHole

replace
  :: Monad m
  => ExpressionSetter m
  -> Data.ExpressionIRef
  -> Transaction ViewTag m Data.ExpressionIRef
replace setExprI newExprI = do
  setExprI newExprI
  return newExprI

replaceWithHole
  :: Monad m
  => ExpressionSetter m
  -> Transaction ViewTag m Data.ExpressionIRef
replaceWithHole setExprI = replace setExprI =<< newHole

lambdaWrap
  :: Monad m
  => Data.ExpressionIRef -> ExpressionSetter m
  -> Transaction ViewTag m Data.ExpressionIRef
lambdaWrap exprI setExprI = do
  newParamTypeI <- newHole
  newExprI <-
    Data.newExprIRef . Data.ExpressionLambda $
    Data.Lambda newParamTypeI exprI
  setExprI newExprI
  return newExprI

redexWrap
  :: Monad m
  => Data.ExpressionIRef -> ExpressionSetter m
  -> Transaction ViewTag m Data.ExpressionIRef
redexWrap exprI setExprI = do
  newParamTypeI <- newHole
  newLambdaI <-
    Data.newExprIRef . Data.ExpressionLambda $
    Data.Lambda newParamTypeI exprI
  newValueI <- newHole
  newApplyI <-
    Data.newExprIRef . Data.ExpressionApply $
    Data.Apply newLambdaI newValueI
  setExprI newApplyI
  return newLambdaI

lambdaTypeSetter
  :: Monad m
  => (Data.LambdaI -> Data.ExpressionI)
  -> Data.ExpressionIRef -> Data.LambdaI -> ExpressionSetter m
lambdaTypeSetter cons lambdaI (Data.Lambda _ bodyI) =
  Data.writeExprIRef lambdaI . cons . flip Data.Lambda bodyI

lambdaBodySetter
  :: Monad m
  => (Data.LambdaI -> Data.ExpressionI)
  -> Data.ExpressionIRef -> Data.LambdaI -> ExpressionSetter m
lambdaBodySetter cons lambdaI (Data.Lambda paramTypeI _) =
  Data.writeExprIRef lambdaI . cons . Data.Lambda paramTypeI

applyFuncSetter
  :: Monad m
  => Data.ExpressionIRef
  -> Data.ApplyI -> ExpressionSetter m
applyFuncSetter applyI (Data.Apply _ argI) =
  Data.writeExprIRef applyI . Data.ExpressionApply . (`Data.Apply` argI)

applyArgSetter
  :: Monad m
  => Data.ExpressionIRef
  -> Data.ApplyI -> ExpressionSetter m
applyArgSetter applyI (Data.Apply funcI _) =
  Data.writeExprIRef applyI . Data.ExpressionApply . Data.Apply funcI
