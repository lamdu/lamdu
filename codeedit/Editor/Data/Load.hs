{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( loadDefinition
  , loadExpression
  , loadPureExpression, exprAddProp
  , loadPureDefinitionType
  ) where

import Control.Monad (liftM, (<=<))
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

type T = Transaction

loadExpression ::
  (Monad n, Monad m) =>
  DataIRef.ExpressionProperty (T t m) ->
  T u n (Data.Expression (DataIRef.ExpressionProperty (T t m)))
loadExpression prop =
  liftM (exprAddProp (Property.set prop)) .
  loadPureExpression $ Property.value prop

-- TODO: -> Rename to loadIRefExpression?
loadPureExpression
  :: Monad m
  => Data.ExpressionIRef -> T t m (Data.Expression Data.ExpressionIRef)
loadPureExpression exprI =
  liftM (`Data.Expression` exprI) .
  Traversable.mapM loadPureExpression =<< DataIRef.readExprBody exprI

loadPureDefinitionType ::
  Monad m => Data.DefinitionIRef -> T t m (Data.Expression Data.ExpressionIRef)
loadPureDefinitionType =
  loadPureExpression . Data.defType <=< Transaction.readIRef

exprAddProp ::
  Monad m =>
  (Data.ExpressionIRef -> T t m ()) ->
  Data.Expression Data.ExpressionIRef ->
  Data.Expression (DataIRef.ExpressionProperty (T t m))
exprAddProp setIRef (Data.Expression body iref) =
  Data.Expression newBody $ Property iref setIRef
  where
    newBody =
      case body of
      Data.ExpressionLambda lambda ->
        Data.ExpressionLambda $ loadLambda Data.ExpressionLambda lambda
      Data.ExpressionPi lambda ->
        Data.ExpressionPi $ loadLambda Data.ExpressionPi lambda
      Data.ExpressionApply (Data.Apply func arg) ->
        Data.makeApply
        (loadSubexpr (`Data.makeApply` Data.ePayload arg) func)
        (loadSubexpr (Data.ePayload func `Data.makeApply` ) arg)
      Data.ExpressionLeaf x -> Data.ExpressionLeaf x
    loadSubexpr f = exprAddProp (DataIRef.writeExprBody iref . f)
    loadLambda cons (Data.Lambda param paramType result) =
      let lam = Data.Lambda param
      in Data.Lambda param
         (loadSubexpr (cons . (`lam` Data.ePayload result)) paramType)
         (loadSubexpr (cons . (Data.ePayload paramType `lam`)) result)

loadDefinition
  :: (Monad m, Monad f)
  => Data.DefinitionIRef
  -> T t m (Data.Definition (Data.Expression (DataIRef.ExpressionProperty (T u f))))
loadDefinition defI = do
  Data.Definition body typeExprI <- Transaction.readIRef defI
  let writeBack = Transaction.writeIRef defI
  defType <-
    liftM (exprAddProp (writeBack . (body `Data.Definition` ))) $
    loadPureExpression typeExprI
  liftM (`Data.Definition` defType) $
    case body of
    Data.DefinitionExpression exprI ->
      liftM
      (Data.DefinitionExpression .
       exprAddProp
       (writeBack . (`Data.Definition` typeExprI) .
        Data.DefinitionExpression)) $
      loadPureExpression exprI
    Data.DefinitionBuiltin (Data.Builtin name) ->
      return . Data.DefinitionBuiltin $ Data.Builtin name
