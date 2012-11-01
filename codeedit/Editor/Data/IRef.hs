module Editor.Data.IRef
  ( ExpressionBody
  , ExpressionProperty, epGuid
  , Lambda, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newLambda, newPi
  , newExpression, writeExpression
  ) where

import Control.Monad (liftM)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data

type Expression = Data.ExpressionIRef

unExpression :: Expression -> IRef ExpressionBody
unExpression = Data.unExpressionIRef

type ExpressionProperty m = Property m Expression

epGuid :: ExpressionProperty m -> Guid
epGuid = IRef.guid . unExpression . Property.value

type ExpressionBody = Data.ExpressionBody Expression
type Lambda = Data.Lambda Expression
type Apply = Data.Apply Expression

exprGuid :: Expression -> Guid
exprGuid = IRef.guid . unExpression

newExprBody :: Monad m => ExpressionBody -> Transaction t m Expression
newExprBody = liftM Data.ExpressionIRef . Transaction.newIRef

newLambdaCons ::
  Monad m =>
  (Guid -> expr -> expr -> ExpressionBody) ->
  expr -> expr -> Transaction t m (Guid, Expression)
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: Monad m => Expression -> Expression -> Transaction t m (Guid, Expression)
newPi = newLambdaCons Data.makePi

newLambda :: Monad m => Expression -> Expression -> Transaction t m (Guid, Expression)
newLambda = newLambdaCons Data.makeLambda

readExprBody :: Monad m => Expression -> Transaction t m ExpressionBody
readExprBody = Transaction.readIRef . unExpression

writeExprBody :: Monad m => Expression -> ExpressionBody -> Transaction t m ()
writeExprBody = Transaction.writeIRef . unExpression

newExpression :: Monad m => Data.Expression a -> Transaction t m Expression
newExpression = liftM (fst . Data.ePayload) . newExpressionFromH

-- Returns expression with new Guids
writeExpression
  :: Monad m => Expression -> Data.Expression a
  -> Transaction t m (Data.Expression (Expression, a))
writeExpression (Data.ExpressionIRef iref) expr = do
  exprBodyP <- expressionBodyFrom expr
  Transaction.writeIRef iref $ fmap (fst . Data.ePayload) exprBodyP
  return $ Data.Expression exprBodyP
    (Data.ExpressionIRef iref, Data.ePayload expr)

expressionBodyFrom ::
  Monad m => Data.Expression a ->
  Transaction t m
  (Data.ExpressionBody (Data.Expression (Expression, a)))
expressionBodyFrom = Traversable.mapM newExpressionFromH . Data.eValue

newExpressionFromH ::
  Monad m =>
  Data.Expression a ->
  Transaction t m (Data.Expression (Expression, a))
newExpressionFromH expr =
  liftM f . Transaction.newIRefWithGuid $ const mkPair
  where
    mkPair = do
      body <- expressionBodyFrom expr
      return (fmap (fst . Data.ePayload) body, body)
    f (exprI, body) =
      Data.Expression body (Data.ExpressionIRef exprI, Data.ePayload expr)
