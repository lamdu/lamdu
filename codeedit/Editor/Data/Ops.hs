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
import qualified Editor.Data.IRef as DataIRef

type T = Transaction ViewTag

giveAsArg ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  T m Data.ExpressionIRef
giveAsArg exprP = do
  newFuncI <- newHole
  Property.set exprP =<<
    DataIRef.newExpr
    (Data.makeApply newFuncI (Property.value exprP))
  return newFuncI

callWithArg ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  T m Data.ExpressionIRef
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    DataIRef.newExpr
    (Data.makeApply (Property.value exprP) argI)
  return argI

newHole :: Monad m => T m Data.ExpressionIRef
newHole = DataIRef.newExpr $ Data.ExpressionLeaf Data.Hole

replace
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> Data.ExpressionIRef
  -> T m Data.ExpressionIRef
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m Data.ExpressionIRef
replaceWithHole exprP = replace exprP =<< newHole

lambdaWrap
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m Data.ExpressionIRef
lambdaWrap exprP = do
  newParamTypeI <- newHole
  newExprI <-
    DataIRef.newExpr .
    Data.makeLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return newExprI

redexWrap
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m Data.ExpressionIRef
redexWrap exprP = do
  newParamTypeI <- newHole
  newLambdaI <-
    DataIRef.newExpr .
    Data.makeLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    DataIRef.newExpr $ Data.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return newLambdaI
