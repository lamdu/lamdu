{-# LANGUAGE DeriveFunctor #-}
module Editor.Data.Ops
  ( newHole, giveAsArg, callWithArg
  , replace, replaceWithHole, lambdaWrap, redexWrap
  )
where

import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Data.Store.Property as Property
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as DataTyped

type T = Transaction ViewTag

toI :: DataTyped.ExpressionRef (Transaction ViewTag m) -> DataTyped.ExpressionIRef
toI = DataTyped.exprIRefFromExpressionRef

giveAsArg
  :: Monad m
  => DataTyped.ExpressionRef (T m)
  -> T m Guid
giveAsArg exprP = do
  newFuncI <- newHole
  _ <-
    DataTyped.erReplaceWithVal exprP . Data.ExpressionApply $
    Data.Apply newFuncI (toI exprP)
  return $ DataTyped.eiGuid newFuncI

callWithArg
  :: Monad m
  => DataTyped.ExpressionRef (T m)
  -> T m Guid
callWithArg exprP = do
  argI <- newHole
  _ <-
    DataTyped.erReplaceWithVal exprP . Data.ExpressionApply $
    Data.Apply (toI exprP) argI
  return $ DataTyped.eiGuid argI

newHole :: Monad m => T m DataTyped.ExpressionIRef
newHole = DataTyped.exprIRefFromVal Data.ExpressionHole

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
  => DataTyped.ExpressionRef (T m)
  -> T m Guid
replaceWithHole exprP = DataTyped.erReplaceWithVal exprP Data.ExpressionHole

lambdaWrap
  :: Monad m
  => DataTyped.ExpressionRef (T m)
  -> T m Guid
lambdaWrap exprP =
  newHole >>=
  DataTyped.erReplaceWithVal exprP . Data.ExpressionLambda .
  (`Data.Lambda` toI exprP)

redexWrap
  :: Monad m
  => DataTyped.ExpressionRef (T m)
  -> T m Guid
redexWrap exprP = do
  newLambdaI <-
    DataTyped.exprIRefFromVal . Data.ExpressionLambda .
    (`Data.Lambda` toI exprP) =<< newHole
  _ <-
    DataTyped.erReplaceWithVal exprP . Data.ExpressionApply .
    Data.Apply newLambdaI =<< newHole
  return $ DataTyped.eiGuid newLambdaI
