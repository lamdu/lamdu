{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.Data.Expr.IRef
  ( Expr, ExprM
  , ExprI(..), ExprIM
  , ExprBody
  , ExprProperty, epGuid
  , Lam, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newLambda, newPi
  , newExpr, writeExpr, readExpr
  , writeExprWithStoredSubexpressions
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
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil

type Expr t = Expr.Expr (DefI t)
type ExprM m = Expr (Tag m)

type T = Transaction

type DefI t = IRef t (Definition.Body (ExprI t))
type DefIM m = DefI (Tag m)

newtype ExprI t = ExprI {
  unExpr :: IRef t (Expr.Body (DefI t) Guid (ExprI t))
  } deriving (Eq, Ord, Show, Typeable, Binary)

type ExprIM m = ExprI (Tag m)

-- TODO: Remove "Expr" prefix from these? We're in a module
-- called Expr.IRef
type ExprProperty m = Property (T m) (ExprIM m)
type ExprBody t = Expr.Body (DefI t) Guid (ExprI t)
type Lam t = Expr.Lam (ExprI t)
type Apply t = Expr.Apply (ExprI t)

epGuid :: ExprProperty m -> Guid
epGuid = IRef.guid . unExpr . Property.value

exprGuid :: ExprI t -> Guid
exprGuid = IRef.guid . unExpr

newExprBody :: MonadA m => ExprBody (Tag m) -> T m (ExprIM m)
newExprBody = fmap ExprI . Transaction.newIRef

newLambdaCons ::
  MonadA m =>
  (Guid -> expr -> expr -> ExprBody (Tag m)) ->
  expr -> expr -> T m (Guid, ExprIM m)
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: MonadA m => ExprIM m -> ExprIM m -> T m (Guid, ExprIM m)
newPi = newLambdaCons ExprUtil.makePi

newLambda :: MonadA m => ExprIM m -> ExprIM m -> T m (Guid, ExprIM m)
newLambda = newLambdaCons ExprUtil.makeLambda

readExprBody :: MonadA m => ExprIM m -> T m (ExprBody (Tag m))
readExprBody = Transaction.readIRef . unExpr

writeExprBody ::
  MonadA m => ExprIM m -> ExprBody (Tag m) -> T m ()
writeExprBody = Transaction.writeIRef . unExpr

newExpr :: MonadA m => ExprM m () -> T m (ExprIM m)
newExpr =
  fmap (^. Expr.ePayload . Lens._1) .
  newExprFromH id . ((,) Nothing <$>)

-- Returns expression with new Guids
writeExpr ::
  MonadA m =>
  (def -> DefIM m) ->
  ExprIM m -> Expr.Expr def a ->
  T m (Expr.Expr def (ExprIM m, a))
writeExpr getDef iref =
  writeExprWithStoredSubexpressions getDef iref .
  fmap ((,) Nothing)

writeExprWithStoredSubexpressions ::
  MonadA m =>
  (def -> DefIM m) ->
  ExprIM m ->
  Expr.Expr def (Maybe (ExprIM m), a) ->
  T m (Expr.Expr def (ExprIM m, a))
writeExprWithStoredSubexpressions getDef iref expr = do
  exprBodyP <- expressionBodyFrom getDef expr
  exprBodyP
    <&> (^. Expr.ePayload . Lens._1)
    & ExprLens.bodyDef %~ getDef
    & writeExprBody iref
  return $ Expr.Expr exprBodyP
    (iref, expr ^. Expr.ePayload . Lens._2)

readExpr ::
  MonadA m => ExprIM m -> T m (ExprM m (ExprIM m))
readExpr exprI =
  fmap (`Expr.Expr` exprI) .
  traverse readExpr =<< readExprBody exprI

expressionBodyFrom ::
  MonadA m =>
  (def -> DefIM m) ->
  Expr.Expr def (Maybe (ExprIM m), a) ->
  T m (Expr.BodyExpr def Guid (ExprIM m, a))
expressionBodyFrom getDef = traverse (newExprFromH getDef) . (^. Expr.eBody)

newExprFromH ::
  MonadA m =>
  (def -> DefIM m) ->
  Expr.Expr def (Maybe (ExprIM m), a) ->
  T m (Expr.Expr def (ExprIM m, a))
newExprFromH getDef expr =
  case mIRef of
  Just iref -> writeExprWithStoredSubexpressions getDef iref expr
  Nothing -> do
    body <- expressionBodyFrom getDef expr
    exprI <-
      body
      <&> (^. Expr.ePayload . Lens._1)
      & ExprLens.bodyDef %~ getDef
      & Transaction.newIRef
    return $ Expr.Expr body (ExprI exprI, pl)
  where
    (mIRef, pl) = expr ^. Expr.ePayload

variableRefGuid :: Expr.VariableRef (DefI t) Guid -> Guid
variableRefGuid (Expr.ParameterRef i) = i
variableRefGuid (Expr.DefinitionRef i) = IRef.guid i

addProperties ::
  MonadA m =>
  (def -> DefIM m) ->
  (ExprIM m -> T m ()) ->
  Expr.Expr def (ExprIM m, a) ->
  Expr.Expr def (ExprProperty m, a)
addProperties getDefI setIRef (Expr.Expr body (iref, a)) =
  Expr.Expr (body & Lens.traversed %@~ f) (Property iref setIRef, a)
  where
    f index =
      addProperties getDefI $ \newIRef ->
      body
      <&> (^. Expr.ePayload . Lens._1) -- convert to body of IRefs
      & Lens.element index .~ newIRef
      & ExprLens.bodyDef %~ getDefI
      & writeExprBody iref
