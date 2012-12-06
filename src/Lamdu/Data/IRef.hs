{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.IRef
  ( Expression(..)
  , ExpressionBody
  , ExpressionProperty, epGuid
  , Lambda, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newLambda, newPi
  , newExpression, writeExpression, readExpression
  , DefI, DefinitionI
  ) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data as Data

type DefinitionI = Data.Definition Expression
type DefI = IRef DefinitionI

type T = Transaction

newtype Expression = Expression {
  unExpression :: IRef (Data.ExpressionBody DefI Expression)
  } deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''Expression

type ExpressionProperty m = Property m Expression
type ExpressionBody = Data.ExpressionBody DefI Expression
type Lambda = Data.Lambda Expression
type Apply = Data.Apply Expression

epGuid :: ExpressionProperty m -> Guid
epGuid = IRef.guid . unExpression . Property.value

exprGuid :: Expression -> Guid
exprGuid = IRef.guid . unExpression

newExprBody :: MonadA m => ExpressionBody -> T m Expression
newExprBody = fmap Expression . Transaction.newIRef

newLambdaCons ::
  MonadA m =>
  (Guid -> expr -> expr -> ExpressionBody) ->
  expr -> expr -> T m (Guid, Expression)
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: MonadA m => Expression -> Expression -> T m (Guid, Expression)
newPi = newLambdaCons Data.makePi

newLambda :: MonadA m => Expression -> Expression -> T m (Guid, Expression)
newLambda = newLambdaCons Data.makeLambda

readExprBody :: MonadA m => Expression -> T m ExpressionBody
readExprBody = Transaction.readIRef . unExpression

writeExprBody :: MonadA m => Expression -> ExpressionBody -> T m ()
writeExprBody = Transaction.writeIRef . unExpression

newExpression :: MonadA m => Data.Expression DefI a -> T m Expression
newExpression = fmap (fst . Lens.view Data.ePayload) . newExpressionFromH

-- Returns expression with new Guids
writeExpression ::
  MonadA m => Expression -> Data.Expression DefI a ->
  T m (Data.Expression DefI (Expression, a))
writeExpression iref expr = do
  exprBodyP <- expressionBodyFrom expr
  writeExprBody iref $ fmap (fst . Lens.view Data.ePayload) exprBodyP
  return $ Data.Expression exprBodyP
    (iref, expr ^. Data.ePayload)

readExpression ::
  MonadA m => Expression -> T m (Data.Expression DefI Expression)
readExpression exprI =
  fmap (`Data.Expression` exprI) .
  traverse readExpression =<< readExprBody exprI

expressionBodyFrom ::
  MonadA m => Data.Expression DefI a ->
  T m (Data.ExpressionBodyExpr DefI (Expression, a))
expressionBodyFrom = traverse newExpressionFromH . Lens.view Data.eValue

newExpressionFromH ::
  MonadA m =>
  Data.Expression DefI a ->
  T m (Data.Expression DefI (Expression, a))
newExpressionFromH expr =
  fmap f . Transaction.newIRefWithGuid $ const mkPair
  where
    mkPair = do
      body <- expressionBodyFrom expr
      return (fmap (fst . Lens.view Data.ePayload) body, body)
    f (exprI, body) =
      Data.Expression body (Expression exprI, expr ^. Data.ePayload)
