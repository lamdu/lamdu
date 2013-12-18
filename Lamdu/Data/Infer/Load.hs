{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Load
  ( Loader(..)
  , Error(..)
  , LoadedDef(..), ldDef, ldType
  , RefData.LoadedBody, LoadedExpr
  , T, load, newDefinition
  , exprIntoContext
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.RefData (LoadedDef(..), ldDef, ldType, LoadedExpr)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.TypedValue (TypedValue(..), tvType)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Either as Either
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.RefData as RefData

newtype Loader def m = Loader
  { loadDefType :: def -> m (Expr.Expression def ())
    -- TODO: For synonyms we'll need loadDefVal
  }

newtype Error def = LoadUntypedDef def
  deriving (Show)

type T def m = StateT (Context def) (EitherT (Error def) m)

exprIntoContext ::
  (Ord def, MonadA m) => RefData.Scope def -> LoadedExpr def () ->
  StateT (Context def) m (ExprRef def)
exprIntoContext scope (Expr.Expression body ()) = do
  newBody <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeRef <- exprIntoContext scope paramType
      paramIdRep <- Lens.zoom Context.guidAliases $ GuidAliases.getRep paramGuid
      Expr.BodyLam . Expr.Lam k paramGuid paramTypeRef <$>
        exprIntoContext (scope & RefData.scopeMap . Lens.at paramIdRep .~ Just paramTypeRef) result
    -- TODO: Assert parameterRefs are not out of scope here
    _ -> body & Lens.traverse %%~ exprIntoContext scope
  Context.fresh scope newBody

-- Error includes untyped def use
loadDefTypeIntoRef :: (Ord def, MonadA m) => Loader def m -> def -> T def m (ExprRef def)
loadDefTypeIntoRef loader@(Loader loadType) def = do
  loadedDefType <- lift . lift $ loadType def
  when (Lens.has ExprLens.holePayloads loadedDefType) .
    lift . Either.left $ LoadUntypedDef def
  exprIntoContext (RefData.Scope mempty Nothing) =<<
    load loader loadedDefType

newDefinition :: (MonadA m, Ord def) => def -> StateT (Context def) m (TypedValue def)
newDefinition def = do
  tv <- TypedValue <$> mkHole <*> mkHole
  Context.defTVs . Lens.at def %= setRef tv
  return tv
  where
    mkHole = Context.freshHole $ RefData.emptyScope def
    setRef tv Nothing = Just tv
    setRef _ (Just _) = error "newDefinition overrides existing def type"

load ::
  (Ord def, MonadA m) =>
  Loader def m -> Expr.Expression def a ->
  T def m (LoadedExpr def a)
load loader expr = expr & ExprLens.exprDef %%~ toLoadedDef
  where
    toLoadedDef def = LoadedDef def . (^. tvType) <$> loadDefTVIfNeeded def
    loadDefTVIfNeeded def = do
      mExistingTV <- Lens.use (Context.defTVs . Lens.at def)
      case mExistingTV of
        Nothing -> do
          newTV <- loadDefTV def
          Context.defTVs . Lens.at def .= Just newTV
          return newTV
        Just tv -> return tv
    loadDefTV def =
      TypedValue
      <$> Context.freshHole (RefData.Scope mempty (Just def))
      <*> loadDefTypeIntoRef loader def
