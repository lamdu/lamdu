{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
  ( newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  , giveAsArgToOperator
  )
where

import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
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
    DataIRef.newExprBody
    (Data.makeApply newFuncI (Property.value exprP))
  return newFuncI

giveAsArgToOperator ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  String ->
  T m Data.ExpressionIRef
giveAsArgToOperator exprP searchTerm = do
  op <- newHole
  (`Property.set` searchTerm) =<< Anchors.assocSearchTermRef (DataIRef.exprGuid op)
  opApplied <- DataIRef.newExprBody . Data.makeApply op $ Property.value exprP
  Property.set exprP =<< DataIRef.newExprBody . Data.makeApply opApplied =<< newHole
  return op

callWithArg ::
  Monad m =>
  DataIRef.ExpressionProperty (T m) ->
  T m Data.ExpressionIRef
callWithArg exprP = do
  argI <- newHole
  Property.set exprP =<<
    DataIRef.newExprBody
    (Data.makeApply (Property.value exprP) argI)
  return argI

newHole :: Monad m => T m Data.ExpressionIRef
newHole = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole

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
  -> T m (Guid, Data.ExpressionIRef)
lambdaWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newExprI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  Property.set exprP newExprI
  return (newParam, newExprI)

redexWrap
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> T m (Guid, Data.ExpressionIRef)
redexWrap exprP = do
  newParamTypeI <- newHole
  (newParam, newLambdaI) <-
    DataIRef.newLambda newParamTypeI $ Property.value exprP
  newValueI <- newHole
  newApplyI <-
    DataIRef.newExprBody $ Data.makeApply newLambdaI newValueI
  Property.set exprP newApplyI
  return (newParam, newLambdaI)
