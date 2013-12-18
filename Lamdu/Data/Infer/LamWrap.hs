module Lamdu.Data.Infer.LamWrap
  ( lambdaWrap
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.Load (LoadedExpr)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, TagParam)
import Lamdu.Data.Infer.TypedValue (TypedValue(..))
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Monad.Run as InferMRun
import qualified Lamdu.Data.Infer.RefData as RefData

addDefScope ::
  (MonadA m, Ord def) =>
  def -> OR.RefMap (TagParam def) (ExprRef def) ->
  StateT (Context def) m ()
addDefScope def newScopeMap = do
  defRefs <-
    Lens.use (Context.defVisibility . Lens.at def)
    <&> (^.. Lens._Just . OR.unsafeRefSetKeys)
  traverse_ addScopeToRef defRefs
  where
    addScopeToRef ref =
      Lens.zoom Context.ufExprs . UFData.modify ref $
      RefData.rdScope . RefData.scopeMap <>~ newScopeMap

-- Only call on a "root" ref that has no parents (otherwise the scope
-- which should be an intersection is wrong):
lamWrapRef ::
  Ord def => Guid -> ExprRef def -> RefData.Scope def ->
  Expr.Kind -> ExprRef def -> Infer def (ExprRef def)
lamWrapRef paramId paramTypeRef scope k defRef = do
  defRep <- InferM.liftUFExprs $ UFData.find defRef
  let lam = Expr.Lam k paramId paramTypeRef defRep
  InferM.liftContext . Context.fresh scope $ Expr.BodyLam lam

lambdaWrap ::
  Ord def =>
  Guid -> ExprRef def ->
  LoadedExpr def (TypedValue def, a) ->
  StateT (Context def) (Either (InferM.Error def))
  (LoadedExpr def (TypedValue def, Maybe a))
lambdaWrap paramId paramTypeRef expr = InferMRun.run $ do
  rootScope <-
    UFData.read rootValRef
    & InferM.liftUFExprs
    <&> (^. RefData.rdScope)
  let
    mkRootExpr body tv =
      Expr.Expression body (tv, Nothing)
    rootDef =
      case rootScope of
        RefData.Scope _ Nothing ->
          error "Cannot lamWrap: Given a non-root of definition"
        RefData.Scope scopMap _ | not $ OR.refMapNull scopMap ->
          error "Cannot lamWrap: Root of definition has elements in scope?!"
        RefData.Scope _ (Just def) -> def
  paramIdRep <- InferM.liftGuidAliases $ GuidAliases.getRep paramId
  InferM.liftContext . addDefScope rootDef $
    OR.refMapSingleton paramIdRep paramTypeRef
  typeRef <- InferM.liftContext . Context.fresh rootScope $ ExprLens.bodyType # ()
  let
    paramTypeSTVExpr =
      mkRootExpr (ExprLens.bodyHole # ()) $ TypedValue paramTypeRef typeRef
    wrap = lamWrapRef paramId paramTypeRef rootScope
  lambdaRef <- wrap Expr.KVal rootValRef
  piRef <- wrap Expr.KType rootTypRef
  return .
    mkRootExpr (ExprUtil.makeLam Expr.KVal paramId paramTypeSTVExpr (expr <&> Lens._2 %~ Just)) $
    TypedValue lambdaRef piRef
  where
    TypedValue rootValRef rootTypRef =
      expr ^. Expr.ePayload . Lens._1
