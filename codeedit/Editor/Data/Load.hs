{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor #-}
module Editor.Data.Load
  ( Stored(..), Loaded
  , ExpressionSetter
  , loadDefinition
  , loadExpressionProperty
  , loadExpressionIRef, exprAddProp
  ) where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

type T = Transaction

type ExpressionSetter m = Data.ExpressionIRef -> m ()

data Stored m a = Stored
  { sSetIRef :: ExpressionSetter m
  , sExpr :: Data.Expression a
  } deriving (Functor)

type Loaded m = Stored m Data.ExpressionIRef

loadExpressionProperty ::
  (Monad n, Monad m) =>
  DataIRef.ExpressionProperty (T m) ->
  T n (Loaded (T m))
loadExpressionProperty prop =
  liftM (Stored (Property.set prop)) .
  loadExpressionIRef $ Property.value prop

loadExpressionIRef :: Monad m => Data.ExpressionIRef -> T m (Data.Expression Data.ExpressionIRef)
loadExpressionIRef exprI =
  liftM (`Data.Expression` exprI) .
  Traversable.mapM loadExpressionIRef =<< DataIRef.readExprBody exprI

exprAddProp ::
  Monad m => Stored (T m) (Data.ExpressionIRef, a) ->
  Data.Expression (DataIRef.ExpressionProperty (T m), a)
exprAddProp (Stored setIRef (Data.Expression body (iref, a))) =
  Data.Expression newBody (Property iref setIRef, a)
  where
    newBody =
      case body of
      Data.ExpressionLambda lambda ->
        Data.ExpressionLambda $ loadLambda Data.ExpressionLambda lambda
      Data.ExpressionPi lambda ->
        Data.ExpressionPi $ loadLambda Data.ExpressionPi lambda
      Data.ExpressionApply (Data.Apply func arg) ->
        Data.makeApply
        (loadSubexpr (`Data.makeApply` fst (arg ^. Data.ePayload)) func)
        (loadSubexpr (fst (func ^. Data.ePayload) `Data.makeApply` ) arg)
      Data.ExpressionLeaf x -> Data.ExpressionLeaf x
    loadSubexpr f = exprAddProp . Stored (DataIRef.writeExprBody iref . f)
    loadLambda cons (Data.Lambda param paramType result) =
      let lam = Data.Lambda param
      in Data.Lambda param
         (loadSubexpr (cons . (`lam` fst (result ^. Data.ePayload))) paramType)
         (loadSubexpr (cons . (fst (paramType ^. Data.ePayload) `lam`)) result)

loadDefinition ::
  (Monad m, Monad f) =>
  Data.DefinitionIRef ->
  T m (Data.Definition (Loaded (T f)))
loadDefinition defI = do
  Data.Definition body typeExprI <- Transaction.readIRef defI
  let
    writeBack = Transaction.writeIRef defI
    writeType = writeBack . (body `Data.Definition` )
  defType <-
    liftM (Stored writeType) $ loadExpressionIRef typeExprI
  liftM (`Data.Definition` defType) $
    case body of
    Data.DefinitionExpression bodyI -> do
      let
        writeBody =
          writeBack . (`Data.Definition` typeExprI) .
          Data.DefinitionExpression
      bodyIRef <- loadExpressionIRef bodyI
      return . Data.DefinitionExpression $
        Stored writeBody bodyIRef
    Data.DefinitionBuiltin (Data.Builtin name) ->
      return . Data.DefinitionBuiltin $ Data.Builtin name
