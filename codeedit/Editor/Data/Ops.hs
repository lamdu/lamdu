{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
  ( newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  )
where

import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import qualified Data.Store.Property as Property
import qualified Editor.Data as Data

type T = Transaction ViewTag

giveAsArg ::
  Monad m =>
  Data.ExpressionIRefProperty (T m) ->
  T m Data.ExpressionIRef
giveAsArg exprP = do
  newFuncI <- newHole
  Property.set exprP =<< (Data.newExprIRef . Data.ExpressionApply) (Data.Apply newFuncI (Property.value exprP))
  return newFuncI

callWithArg ::
  Monad m =>
  Data.ExpressionIRefProperty (T m) ->
  T m Data.ExpressionIRef
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    (Data.newExprIRef . Data.ExpressionApply)
    (Data.Apply (Property.value exprP) argI)
  return argI

newHole :: Monad m => T m Data.ExpressionIRef
newHole = Data.newExprIRef Data.ExpressionHole

replace
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> Data.ExpressionIRef
  -> T m Data.ExpressionIRef
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> T m Data.ExpressionIRef
replaceWithHole exprP = replace exprP =<< newHole

lambdaWrap
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> T m Data.ExpressionIRef
lambdaWrap exprP = do
  newParamTypeI <- newHole
  newExprI <-
    Data.newExprIRef . Data.ExpressionLambda .
    Data.Lambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return newExprI

redexWrap
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> T m Data.ExpressionIRef
redexWrap exprP = do
  newParamTypeI <- newHole
  newLambdaI <-
    Data.newExprIRef . Data.ExpressionLambda .
    Data.Lambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    Data.newExprIRef . Data.ExpressionApply $
    Data.Apply newLambdaI newValueI
  Property.set exprP newApplyI
  return newLambdaI
