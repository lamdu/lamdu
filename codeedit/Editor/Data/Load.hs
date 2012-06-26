{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( guid
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

-- TODO: ExpressionEntity -> ExpressionEntity
data ExpressionEntity m = ExpressionEntity
  { entityStored :: Data.ExpressionIRefProperty m
  , entityValue :: Data.Expression (ExpressionEntity m)
  }

data DefinitionEntity m = DefinitionEntity
  { defEntityIRef :: Data.DefinitionIRef
  , defEntityValue :: Data.Definition (ExpressionEntity m)
  }

type T = Transaction ViewTag

guid :: ExpressionEntity m -> Guid
guid = IRef.guid . Data.unExpressionIRef . Property.value . entityStored

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
    Data.ExpressionApply apply ->
      liftM Data.ExpressionApply $
      liftM2 Data.Apply
      (loadExpression (applyFuncProp exprI apply))
      (loadExpression (applyArgProp exprI apply))
    Data.ExpressionGetVariable x -> return $ Data.ExpressionGetVariable x
    Data.ExpressionLiteralInteger x -> return $ Data.ExpressionLiteralInteger x
    Data.ExpressionHole -> return Data.ExpressionHole
    Data.ExpressionMagic -> return Data.ExpressionMagic
    Data.ExpressionBuiltin bi -> return $ Data.ExpressionBuiltin bi
  where
    exprI = Property.value exprP
    loadLambda cons lambda =
      liftM2 Data.Lambda
      (loadExpression (lambdaTypeProp cons exprI lambda))
      (loadExpression (lambdaBodyProp cons exprI lambda))

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

lambdaTypeProp
  :: Monad m
  => (Data.LambdaI -> Data.ExpressionI)
  -> Data.ExpressionIRef -> Data.LambdaI
  -> Data.ExpressionIRefProperty (T m)
lambdaTypeProp cons lambdaI (Data.Lambda paramTypeI bodyI) =
  Property paramTypeI
  (Data.writeExprIRef lambdaI . cons . flip Data.Lambda bodyI)

lambdaBodyProp
  :: Monad m
  => (Data.LambdaI -> Data.ExpressionI)
  -> Data.ExpressionIRef -> Data.LambdaI
  -> Data.ExpressionIRefProperty (T m)
lambdaBodyProp cons lambdaI (Data.Lambda paramTypeI bodyI) =
  Property bodyI
  (Data.writeExprIRef lambdaI . cons . Data.Lambda paramTypeI)

applyFuncProp
  :: Monad m
  => Data.ExpressionIRef
  -> Data.ApplyI -> Data.ExpressionIRefProperty (T m)
applyFuncProp applyI (Data.Apply funcI argI) =
  Property funcI
  (Data.writeExprIRef applyI . Data.ExpressionApply . (`Data.Apply` argI))

applyArgProp
  :: Monad m
  => Data.ExpressionIRef
  -> Data.ApplyI -> Data.ExpressionIRefProperty (T m)
applyArgProp applyI (Data.Apply funcI argI) =
  Property argI
  (Data.writeExprIRef applyI . Data.ExpressionApply . Data.Apply funcI)
