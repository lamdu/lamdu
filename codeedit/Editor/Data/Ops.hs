{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
  ( newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  , giveAsArgToOperator
  )
where

import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

type T = Transaction

giveAsArg ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  T m DataIRef.Expression
giveAsArg exprP = do
  newFuncI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply newFuncI (Property.value exprP))
  return newFuncI

giveAsArgToOperator ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  String ->
  T m DataIRef.Expression
giveAsArgToOperator exprP searchTerm = do
  op <- newHole
  (`Property.set` searchTerm) =<< Anchors.assocSearchTermRef (DataIRef.exprGuid op)
  opApplied <- DataIRef.newExprBody . Data.makeApply op $ Property.value exprP
  Property.set exprP =<< DataIRef.newExprBody . Data.makeApply opApplied =<< newHole
  return op

callWithArg ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  T m DataIRef.Expression
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply (Property.value exprP) argI)
  return argI

newHole :: Monad m => T m DataIRef.Expression
newHole = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole

replace
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> DataIRef.Expression
  -> T m DataIRef.Expression
replace exprP newExprI = do
  Property.set exprP newExprI
  return newExprI

replaceWithHole
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m DataIRef.Expression
replaceWithHole exprP = replace exprP =<< newHole

lambdaWrap
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m (Guid, DataIRef.Expression)
lambdaWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newExprI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m (Guid, DataIRef.Expression)
redexWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newLambdaI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    DataIRef.newExprBody $ Data.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)
