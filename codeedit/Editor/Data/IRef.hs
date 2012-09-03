module Editor.Data.IRef
  ( ExpressionBody
  , ExpressionProperty, epGuid
  , Lambda, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newExpression, writeExpression
  ) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
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

readExprBody :: Monad m => Expression -> Transaction t m ExpressionBody
readExprBody = Transaction.readIRef . unExpression

writeExprBody :: Monad m => Expression -> ExpressionBody -> Transaction t m ()
writeExprBody = Transaction.writeIRef . unExpression

type Scope = [(Guid, Guid)]

newExpression ::
  Monad m => Data.Expression a -> Transaction t m Expression
newExpression =
  liftM fst . newExpressionFromH []

-- Returns expression with new Guids
writeExpression
  :: Monad m => Expression -> Data.Expression a
  -> Transaction t m (Data.Expression a)
writeExpression (Data.ExpressionIRef iref) expr = do
  (exprBodyI, exprBodyP) <- expressionBodyFrom [] g expr
  Transaction.writeIRef iref exprBodyI
  return . Data.Expression g exprBodyP $ Data.ePayload expr
  where
    g = IRef.guid iref

expressionBodyFrom ::
  Monad m =>
  Scope -> Guid -> Data.Expression a ->
  Transaction t m (ExpressionBody, Data.ExpressionBody (Data.Expression a))
expressionBodyFrom scope newGuid (Data.Expression oldGuid expr _) =
  liftM f .
  Traversable.mapM (newExpressionFromH newScope) $
  case expr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef parGuid)) ->
    Data.makeParameterRef . fromMaybe parGuid $ lookup parGuid newScope
  x -> x
  where
    newScope
      | hasLambda expr = (oldGuid, newGuid) : scope
      | otherwise = scope
    f body = (fmap fst body, fmap snd body)

newExpressionFromH ::
  Monad m =>
  Scope -> Data.Expression a ->
  Transaction t m (Expression, Data.Expression a)
newExpressionFromH scope expr =
  liftM (f . first Data.ExpressionIRef) . Transaction.newIRefWithGuid $
  flip (expressionBodyFrom scope) expr
  where
    f (exprI, body) = (exprI, Data.Expression (exprGuid exprI) body (Data.ePayload expr))

hasLambda :: Data.ExpressionBody a -> Bool
hasLambda Data.ExpressionPi {} = True
hasLambda Data.ExpressionLambda {} = True
hasLambda _ = False
