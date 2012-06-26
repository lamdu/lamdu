{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( StoredExpressionRef
  , guid, esGuid
  , loadDefinition, DefinitionEntity(..)
  , loadExpression, ExpressionEntity(..)
  )
where

import Control.Monad (liftM, liftM2)
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps

type StoredExpressionRef m = Property m Data.ExpressionIRef

-- TODO: ExpressionEntity -> ExpressionEntity
data ExpressionEntity m = ExpressionEntity
  { entityStored :: StoredExpressionRef m
  , entityValue :: Data.Expression (ExpressionEntity m)
  }

data DefinitionEntity m = DefinitionEntity
  { defEntityIRef :: Data.DefinitionIRef
  , defEntityValue :: Data.Definition (ExpressionEntity m)
  }

type T = Transaction ViewTag

esGuid :: StoredExpressionRef m -> Guid
esGuid = IRef.guid . Data.unExpressionIRef . Property.value

guid :: ExpressionEntity m -> Guid
guid = esGuid . entityStored

loadExpression
  :: (Monad m, Monad f)
  => Property (T f) Data.ExpressionIRef
  -> T m (ExpressionEntity (T f))
loadExpression exprP = do
  expr <- Data.readExprIRef exprI
  liftM (ExpressionEntity exprP) $
    case expr of
    Data.ExpressionLambda lambda ->
      liftM Data.ExpressionLambda $ loadLambda Data.ExpressionLambda lambda
    Data.ExpressionPi lambda ->
      liftM Data.ExpressionPi $ loadLambda Data.ExpressionPi lambda
    Data.ExpressionApply apply@(Data.Apply funcI argI) ->
      liftM Data.ExpressionApply $
      liftM2 Data.Apply
      (loadExpression (Property funcI (DataOps.applyFuncSetter exprI apply)))
      (loadExpression (Property argI (DataOps.applyArgSetter exprI apply)))
    Data.ExpressionGetVariable x -> return $ Data.ExpressionGetVariable x
    Data.ExpressionLiteralInteger x -> return $ Data.ExpressionLiteralInteger x
    Data.ExpressionHole -> return Data.ExpressionHole
    Data.ExpressionMagic -> return Data.ExpressionMagic
    Data.ExpressionBuiltin bi -> return $ Data.ExpressionBuiltin bi
  where
    exprI = Property.value exprP
    loadLambda cons lambda@(Data.Lambda argType body) =
      liftM2 Data.Lambda
      (loadExpression
       (Property argType (DataOps.lambdaTypeSetter cons exprI lambda)))
      (loadExpression
       (Property body (DataOps.lambdaBodySetter cons exprI lambda)))

loadDefinition
  :: (Monad m, Monad f)
  => Data.DefinitionIRef
  -> T m (DefinitionEntity (T f))
loadDefinition defI = do
  def <- Transaction.readIRef defI
  liftM (DefinitionEntity defI) $
    case def of
    Data.Definition typeI bodyI -> do
      loadedType <-
        loadExpression . Property typeI $
        Transaction.writeIRef defI . flip Data.Definition bodyI
      loadedExpr <-
        loadExpression . Property bodyI $
        Transaction.writeIRef defI . Data.Definition typeI
      return $ Data.Definition loadedType loadedExpr
