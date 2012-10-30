{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( loadDefinition, DefinitionEntity
  , loadExpression, ExpressionEntity
  , loadPureExpression, loadPureDefinition
  , loadPureDefinitionType
  ) where

import Control.Monad (liftM, liftM2, (<=<))
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

type ExpressionEntity m = Data.Expression (DataIRef.ExpressionProperty m)
type DefinitionEntity m = Data.Definition (ExpressionEntity m)

type T = Transaction ViewTag

loadPureExpression
  :: Monad m
  => Data.ExpressionIRef -> Transaction t m Data.PureExpression
loadPureExpression exprI =
  liftM (Data.pureExpression (DataIRef.exprGuid exprI)) .
  Traversable.mapM loadPureExpression =<< DataIRef.readExprBody exprI

loadPureDefinition
  :: Monad m
  => Data.DefinitionIRef
  -> T m (Data.Definition Data.PureExpression)
loadPureDefinition defI =
  Traversable.mapM loadPureExpression =<< Transaction.readIRef defI

loadPureDefinitionType :: Monad m => Data.DefinitionIRef -> T m Data.PureExpression
loadPureDefinitionType =
  loadPureExpression . Data.defType <=< Transaction.readIRef

loadExpression
  :: (Monad m, Monad f)
  => Property (T f) Data.ExpressionIRef
  -> T m (ExpressionEntity (T f))
loadExpression exprP = do
  expr <- DataIRef.readExprBody exprI
  liftM (flip (Data.Expression (DataIRef.exprGuid exprI)) exprP) $
    case expr of
    Data.ExpressionLambda lambda ->
      liftM Data.ExpressionLambda $ loadLambda Data.ExpressionLambda lambda
    Data.ExpressionPi lambda ->
      liftM Data.ExpressionPi $ loadLambda Data.ExpressionPi lambda
    Data.ExpressionApply apply ->
      liftM2 Data.makeApply
      (loadExpression (applyFuncProp exprI apply))
      (loadExpression (applyArgProp exprI apply))
    Data.ExpressionLeaf x -> return $ Data.ExpressionLeaf x
  where
    exprI = Property.value exprP
    loadLambda cons lambda =
      liftM2 (Data.Lambda (Data.lambdaParamId lambda))
      (loadExpression (lambdaTypeProp cons exprI lambda))
      (loadExpression (lambdaBodyProp cons exprI lambda))

loadDefinition
  :: (Monad m, Monad f)
  => Data.DefinitionIRef
  -> T m (DefinitionEntity (T f))
loadDefinition defI = do
  Data.Definition body typeExprI <- Transaction.readIRef defI
  let writeBack = Transaction.writeIRef defI
  defType <-
    loadExpression $
    Property typeExprI (writeBack . Data.Definition body)
  liftM (`Data.Definition` defType) $
    case body of
    Data.DefinitionExpression exprI ->
      liftM Data.DefinitionExpression . loadExpression $
      Property exprI
      (writeBack .
       (`Data.Definition` typeExprI) . Data.DefinitionExpression)
    Data.DefinitionBuiltin (Data.Builtin name) ->
      return . Data.DefinitionBuiltin $ Data.Builtin name

lambdaTypeProp
  :: Monad m
  => (DataIRef.Lambda -> DataIRef.ExpressionBody)
  -> Data.ExpressionIRef -> DataIRef.Lambda
  -> DataIRef.ExpressionProperty (T m)
lambdaTypeProp cons lambdaI (Data.Lambda param paramTypeI bodyI) =
  Property paramTypeI
  (DataIRef.writeExprBody lambdaI . cons . flip (Data.Lambda param) bodyI)

lambdaBodyProp
  :: Monad m
  => (DataIRef.Lambda -> DataIRef.ExpressionBody)
  -> Data.ExpressionIRef -> DataIRef.Lambda
  -> DataIRef.ExpressionProperty (T m)
lambdaBodyProp cons lambdaI (Data.Lambda param paramTypeI bodyI) =
  Property bodyI
  (DataIRef.writeExprBody lambdaI . cons . Data.Lambda param paramTypeI)

applyFuncProp
  :: Monad m
  => Data.ExpressionIRef
  -> DataIRef.Apply -> DataIRef.ExpressionProperty (T m)
applyFuncProp applyI (Data.Apply funcI argI) =
  Property funcI
  (DataIRef.writeExprBody applyI . (`Data.makeApply` argI))

applyArgProp
  :: Monad m
  => Data.ExpressionIRef
  -> DataIRef.Apply -> DataIRef.ExpressionProperty (T m)
applyArgProp applyI (Data.Apply funcI argI) =
  Property argI
  (DataIRef.writeExprBody applyI . Data.makeApply funcI)
