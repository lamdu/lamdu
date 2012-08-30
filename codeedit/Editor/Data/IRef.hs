module Editor.Data.IRef
  ( ExpressionBody
  , ExpressionProperty, epGuid
  , Lambda, Apply
  , newExpr, readExpr, writeExpr, exprGuid
  , newExpressionFromPure, writeExpressionFromPure
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

newExpr :: Monad m => ExpressionBody -> Transaction t m Expression
newExpr = liftM Data.ExpressionIRef . Transaction.newIRef

readExpr :: Monad m => Expression -> Transaction t m ExpressionBody
readExpr = Transaction.readIRef . unExpression

writeExpr :: Monad m => Expression -> ExpressionBody -> Transaction t m ()
writeExpr = Transaction.writeIRef . unExpression

type Scope = [(Guid, Guid)]

newExpressionFromPure ::
  Monad m => Data.PureExpression -> Transaction t m Expression
newExpressionFromPure =
  liftM fst . newExpressionFromPureH []

-- Returns expression with new Guids
writeExpressionFromPure
  :: Monad m => Expression -> Data.PureExpression
  -> Transaction t m Data.PureExpression
writeExpressionFromPure (Data.ExpressionIRef iref) expr = do
  (exprBodyI, exprBodyP) <- expressionBodyFromPure [] g expr
  Transaction.writeIRef iref exprBodyI
  return $ Data.pureExpression g exprBodyP
  where
    g = IRef.guid iref

expressionBodyFromPure ::
  Monad m =>
  Scope -> Guid -> Data.PureExpression ->
  Transaction t m (ExpressionBody, Data.ExpressionBody Data.PureExpression)
expressionBodyFromPure scope newGuid (Data.Expression oldGuid expr ()) =
  liftM f .
  Traversable.mapM (newExpressionFromPureH newScope) $
  case expr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef parGuid)) ->
    Data.makeParameterRef . fromMaybe parGuid $ lookup parGuid newScope
  x -> x
  where
    newScope
      | hasLambda expr = (oldGuid, newGuid) : scope
      | otherwise = scope
    f body = (fmap fst body, fmap snd body)

newExpressionFromPureH ::
  Monad m =>
  Scope -> Data.PureExpression ->
  Transaction t m (Expression, Data.PureExpression)
newExpressionFromPureH scope =
  liftM (f . first Data.ExpressionIRef) . Transaction.newIRefWithGuid .
  flip (expressionBodyFromPure scope)
  where
    f (exprI, body) = (exprI, Data.pureExpression (exprGuid exprI) body)

hasLambda :: Data.ExpressionBody a -> Bool
hasLambda Data.ExpressionPi {} = True
hasLambda Data.ExpressionLambda {} = True
hasLambda _ = False
