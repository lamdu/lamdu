{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.Data.Expression.IRef
  ( Expression, ExpressionM
  , ExpressionI(..), ExpressionIM
  , ExpressionBody
  , ExpressionProperty, epGuid
  , Lam, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newLambda, newPi
  , newExpression, writeExpression, readExpression
  , writeExpressionWithStoredSubexpressions
  , DefI, DefIM
  , variableRefGuid
  , addProperties
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type Expression t = Expr.Expression (DefI t)
type ExpressionM m = Expression (Tag m)

type T = Transaction

type DefI t = IRef t (Definition.Body (ExpressionI t))
type DefIM m = DefI (Tag m)

newtype ExpressionI t = ExpressionI {
  unExpression :: IRef t (Expr.Body (DefI t) (ExpressionI t))
  } deriving (Eq, Ord, Show, Typeable, Binary)

type ExpressionIM m = ExpressionI (Tag m)

-- TODO: Remove "Expression" prefix from these? We're in a module
-- called Expression.IRef
type ExpressionProperty m = Property (T m) (ExpressionIM m)
type ExpressionBody t = Expr.Body (DefI t) (ExpressionI t)
type Lam t = Expr.Lam (ExpressionI t)
type Apply t = Expr.Apply (ExpressionI t)

epGuid :: ExpressionProperty m -> Guid
epGuid = IRef.guid . unExpression . Property.value

exprGuid :: ExpressionI t -> Guid
exprGuid = IRef.guid . unExpression

newExprBody :: MonadA m => ExpressionBody (Tag m) -> T m (ExpressionIM m)
newExprBody = fmap ExpressionI . Transaction.newIRef

newLambdaCons ::
  MonadA m =>
  (Guid -> expr -> expr -> ExpressionBody (Tag m)) ->
  expr -> expr -> T m (Guid, ExpressionIM m)
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: MonadA m => ExpressionIM m -> ExpressionIM m -> T m (Guid, ExpressionIM m)
newPi = newLambdaCons ExprUtil.makePi

newLambda :: MonadA m => ExpressionIM m -> ExpressionIM m -> T m (Guid, ExpressionIM m)
newLambda = newLambdaCons ExprUtil.makeLambda

readExprBody :: MonadA m => ExpressionIM m -> T m (ExpressionBody (Tag m))
readExprBody = Transaction.readIRef . unExpression

writeExprBody ::
  MonadA m => ExpressionIM m -> ExpressionBody (Tag m) -> T m ()
writeExprBody = Transaction.writeIRef . unExpression

newExpression :: MonadA m => ExpressionM m () -> T m (ExpressionIM m)
newExpression =
  fmap (^. Expr.ePayload . Lens._1) .
  newExpressionFromH id . ((,) Nothing <$>)

-- Returns expression with new Guids
writeExpression ::
  MonadA m =>
  (def -> DefIM m) ->
  ExpressionIM m -> Expr.Expression def a ->
  T m (Expr.Expression def (ExpressionIM m, a))
writeExpression getDef iref =
  writeExpressionWithStoredSubexpressions getDef iref .
  fmap ((,) Nothing)

writeExpressionWithStoredSubexpressions ::
  MonadA m =>
  (def -> DefIM m) ->
  ExpressionIM m ->
  Expr.Expression def (Maybe (ExpressionIM m), a) ->
  T m (Expr.Expression def (ExpressionIM m, a))
writeExpressionWithStoredSubexpressions getDef iref expr = do
  exprBodyP <- expressionBodyFrom getDef expr
  exprBodyP
    <&> (^. Expr.ePayload . Lens._1)
    & ExprLens.bodyDef %~ getDef
    & writeExprBody iref
  return $ Expr.Expression exprBodyP
    (iref, expr ^. Expr.ePayload . Lens._2)

readExpression ::
  MonadA m => ExpressionIM m -> T m (ExpressionM m (ExpressionIM m))
readExpression exprI =
  fmap (`Expr.Expression` exprI) .
  traverse readExpression =<< readExprBody exprI

expressionBodyFrom ::
  MonadA m =>
  (def -> DefIM m) ->
  Expr.Expression def (Maybe (ExpressionIM m), a) ->
  T m (Expr.BodyExpr def (ExpressionIM m, a))
expressionBodyFrom getDef = traverse (newExpressionFromH getDef) . (^. Expr.eBody)

newExpressionFromH ::
  MonadA m =>
  (def -> DefIM m) ->
  Expr.Expression def (Maybe (ExpressionIM m), a) ->
  T m (Expr.Expression def (ExpressionIM m, a))
newExpressionFromH getDef expr =
  case mIRef of
  Just iref -> writeExpressionWithStoredSubexpressions getDef iref expr
  Nothing -> do
    body <- expressionBodyFrom getDef expr
    exprI <-
      body
      <&> (^. Expr.ePayload . Lens._1)
      & ExprLens.bodyDef %~ getDef
      & Transaction.newIRef
    return $ Expr.Expression body (ExpressionI exprI, pl)
  where
    (mIRef, pl) = expr ^. Expr.ePayload

variableRefGuid :: Expr.VariableRef (DefI t) -> Guid
variableRefGuid (Expr.ParameterRef i) = i
variableRefGuid (Expr.DefinitionRef i) = IRef.guid i

addProperties ::
  MonadA m =>
  (def -> DefIM m) ->
  (ExpressionIM m -> T m ()) ->
  Expr.Expression def (ExpressionIM m, a) ->
  Expr.Expression def (ExpressionProperty m, a)
addProperties getDefI setIRef (Expr.Expression body (iref, a)) =
  Expr.Expression (body & Lens.traversed %@~ f) (Property iref setIRef, a)
  where
    f index =
      addProperties getDefI $ \newIRef ->
      body
      <&> (^. Expr.ePayload . Lens._1) -- convert to body of IRefs
      & Lens.element index .~ newIRef
      & ExprLens.bodyDef %~ getDefI
      & writeExprBody iref
