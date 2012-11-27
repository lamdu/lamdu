{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Editor.Data.IRef
  ( Expression(..)
  , ExpressionBody
  , ExpressionProperty, epGuid
  , Lambda, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newLambda, newPi
  , newExpression, writeExpression
  , DefinitionIRef, DefinitionI
  ) where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data

type DefinitionI = Data.Definition Expression
type DefinitionIRef = IRef DefinitionI

newtype Expression = Expression {
  unExpression :: IRef (Data.ExpressionBody DefinitionIRef Expression)
  } deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''Expression

type ExpressionProperty m = Property m Expression
type ExpressionBody = Data.ExpressionBody DefinitionIRef Expression
type Lambda = Data.Lambda Expression
type Apply = Data.Apply Expression

epGuid :: ExpressionProperty m -> Guid
epGuid = IRef.guid . unExpression . Property.value

exprGuid :: Expression -> Guid
exprGuid = IRef.guid . unExpression

newExprBody :: Monad m => ExpressionBody -> Transaction m Expression
newExprBody = liftM Expression . Transaction.newIRef

newLambdaCons ::
  Monad m =>
  (Guid -> expr -> expr -> ExpressionBody) ->
  expr -> expr -> Transaction m (Guid, Expression)
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: Monad m => Expression -> Expression -> Transaction m (Guid, Expression)
newPi = newLambdaCons Data.makePi

newLambda :: Monad m => Expression -> Expression -> Transaction m (Guid, Expression)
newLambda = newLambdaCons Data.makeLambda

readExprBody :: Monad m => Expression -> Transaction m ExpressionBody
readExprBody = Transaction.readIRef . unExpression

writeExprBody :: Monad m => Expression -> ExpressionBody -> Transaction m ()
writeExprBody = Transaction.writeIRef . unExpression

newExpression :: Monad m => Data.Expression DefinitionIRef a -> Transaction m Expression
newExpression = liftM (fst . Lens.view Data.ePayload) . newExpressionFromH

-- Returns expression with new Guids
writeExpression
  :: Monad m => Expression -> Data.Expression DefinitionIRef a
  -> Transaction m (Data.Expression DefinitionIRef (Expression, a))
writeExpression (Expression iref) expr = do
  exprBodyP <- expressionBodyFrom expr
  Transaction.writeIRef iref $ fmap (fst . Lens.view Data.ePayload) exprBodyP
  return $ Data.Expression exprBodyP
    (Expression iref, expr ^. Data.ePayload)

expressionBodyFrom ::
  Monad m =>
  Data.Expression DefinitionIRef a ->
  Transaction m
  (Data.ExpressionBody DefinitionIRef
   (Data.Expression DefinitionIRef (Expression, a)))
expressionBodyFrom = Traversable.mapM newExpressionFromH . Lens.view Data.eValue

newExpressionFromH ::
  Monad m =>
  Data.Expression DefinitionIRef a ->
  Transaction m
  (Data.Expression DefinitionIRef (Expression, a))
newExpressionFromH expr =
  liftM f . Transaction.newIRefWithGuid $ const mkPair
  where
    mkPair = do
      body <- expressionBodyFrom expr
      return (fmap (fst . Lens.view Data.ePayload) body, body)
    f (exprI, body) =
      Data.Expression body (Expression exprI, expr ^. Data.ePayload)
