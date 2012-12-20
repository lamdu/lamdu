{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.Data.IRef
  ( ExpressionI(..)
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
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data as Data

type DefinitionI t = Data.Definition (ExpressionI t)
type DefI t = IRef t (DefinitionI t)

type T = Transaction

newtype ExpressionI t = ExpressionI {
  unExpression :: IRef t (Data.ExpressionBody (DefI t) (ExpressionI t))
  } deriving (Eq, Ord, Show, Typeable, Binary)

type ExpressionProperty m = Property (T m) (ExpressionI (Tag m))
type ExpressionBody t = Data.ExpressionBody (DefI t) (ExpressionI t)
type Lambda t = Data.Lambda (ExpressionI t)
type Apply t = Data.Apply (ExpressionI t)

epGuid :: ExpressionProperty m -> Guid
epGuid = IRef.guid . unExpression . Property.value

exprGuid :: ExpressionI t -> Guid
exprGuid = IRef.guid . unExpression

newExprBody :: MonadA m => ExpressionBody (Tag m) -> T m (ExpressionI (Tag m))
newExprBody = fmap ExpressionI . Transaction.newIRef

newLambdaCons ::
  MonadA m =>
  (Guid -> expr -> expr -> ExpressionBody (Tag m)) ->
  expr -> expr -> T m (Guid, ExpressionI (Tag m))
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: MonadA m => ExpressionI (Tag m) -> ExpressionI (Tag m) -> T m (Guid, ExpressionI (Tag m))
newPi = newLambdaCons Data.makePi

newLambda :: MonadA m => ExpressionI (Tag m) -> ExpressionI (Tag m) -> T m (Guid, ExpressionI (Tag m))
newLambda = newLambdaCons Data.makeLambda

readExprBody :: MonadA m => ExpressionI (Tag m) -> T m (ExpressionBody (Tag m))
readExprBody = Transaction.readIRef . unExpression

writeExprBody :: MonadA m => ExpressionI (Tag m) -> ExpressionBody (Tag m) -> T m ()
writeExprBody = Transaction.writeIRef . unExpression

newExpression :: MonadA m => Data.Expression (DefI (Tag m)) a -> T m (ExpressionI (Tag m))
newExpression = fmap (fst . Lens.view Data.ePayload) . newExpressionFromH

-- Returns expression with new Guids
writeExpression ::
  MonadA m => ExpressionI (Tag m) -> Data.Expression (DefI (Tag m)) a ->
  T m (Data.Expression (DefI (Tag m)) (ExpressionI (Tag m), a))
writeExpression iref expr = do
  exprBodyP <- expressionBodyFrom expr
  writeExprBody iref $ fmap (fst . Lens.view Data.ePayload) exprBodyP
  return $ Data.Expression exprBodyP
    (iref, expr ^. Data.ePayload)

readExpression ::
  MonadA m => ExpressionI (Tag m) -> T m (Data.Expression (DefI (Tag m)) (ExpressionI (Tag m)))
readExpression exprI =
  fmap (`Data.Expression` exprI) .
  traverse readExpression =<< readExprBody exprI

expressionBodyFrom ::
  MonadA m => Data.Expression (DefI (Tag m)) a ->
  T m (Data.ExpressionBodyExpr (DefI (Tag m)) (ExpressionI (Tag m), a))
expressionBodyFrom = traverse newExpressionFromH . Lens.view Data.eValue

newExpressionFromH ::
  MonadA m =>
  Data.Expression (DefI (Tag m)) a ->
  T m (Data.Expression (DefI (Tag m)) (ExpressionI (Tag m), a))
newExpressionFromH expr =
  fmap f . Transaction.newIRefWithGuid $ const mkPair
  where
    mkPair = do
      body <- expressionBodyFrom expr
      return (fmap (fst . Lens.view Data.ePayload) body, body)
    f (exprI, body) =
      Data.Expression body (ExpressionI exprI, expr ^. Data.ePayload)
