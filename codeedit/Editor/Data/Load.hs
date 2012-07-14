{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( loadDefinition, DefinitionEntity(..)
  , loadExpression, ExpressionEntity(..)
  )
where

import Control.Monad (liftM, liftM2)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

data ExpressionEntity m = ExpressionEntity
  { entityStored :: Data.ExpressionIRefProperty m
  , entityValue :: Data.Expression (ExpressionEntity m)
  }

data DefinitionEntity m = DefinitionEntity
  { defEntityIRef :: Data.DefinitionIRef
  , defEntityValue :: Data.Definition (ExpressionEntity m)
  }

type T = Transaction ViewTag

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
    Data.ExpressionBuiltin bi ->
      liftM (Data.ExpressionBuiltin . Data.Builtin (Data.bName bi)) .
      loadExpression $ builtinTypeProp exprI bi
    Data.ExpressionGetVariable x -> return $ Data.ExpressionGetVariable x
    Data.ExpressionLiteralInteger x -> return $ Data.ExpressionLiteralInteger x
    Data.ExpressionHole -> return Data.ExpressionHole
    Data.ExpressionMagic -> return Data.ExpressionMagic
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
    liftM2 Data.Definition
    (loadExpression (defBodyProp defI def))
    (loadExpression (defTypeProp defI def))

defTypeProp
  :: Monad m
  => Data.DefinitionIRef -> Data.DefinitionI
  -> Data.ExpressionIRefProperty (T m)
defTypeProp defI (Data.Definition bodyI typeI) =
  Property typeI
  (Transaction.writeIRef defI . Data.Definition bodyI)

defBodyProp
  :: Monad m
  => Data.DefinitionIRef -> Data.DefinitionI
  -> Data.ExpressionIRefProperty (T m)
defBodyProp defI (Data.Definition bodyI typeI) =
  Property bodyI
  (Transaction.writeIRef defI . flip Data.Definition typeI)

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

builtinTypeProp
  :: Monad m
  => Data.ExpressionIRef
  -> Data.Builtin Data.ExpressionIRef -> Data.ExpressionIRefProperty (T m)
builtinTypeProp builtinI (Data.Builtin name t) =
  Property t
  (Data.writeExprIRef builtinI . Data.ExpressionBuiltin . Data.Builtin name)
