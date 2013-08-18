module Lamdu.Data.Infer.LamWrap
  ( lambdaWrap
  ) where

import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.Decycle (runDecycleT)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Load (LoadedDef)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, TagParam)
import Lamdu.Data.Infer.TypedValue (ScopedTypedValue(..), TypedValue(..))
import Lamdu.Data.Infer.Unify (U, uInfer, decycleDefend)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData

addScopeRecurse ::
  Eq def => def -> OR.RefMap (TagParam def) (ExprRef def) ->
  ExprRef def -> U def ()
addScopeRecurse def newScope ref =
  decycleDefend ref $ \rep -> do
    repData <- uInfer . InferM.liftUFExprs . State.gets $ UFData.readRep rep
    when (repData ^. RefData.rdScope . RefData.scopeMDef == Just def) $ do
      traverse_ (addScopeRecurse def newScope) $ repData ^. RefData.rdBody
      repData
        & RefData.rdScope . RefData.scopeMap <>~ newScope
        & uInfer . InferM.liftUFExprs . UFData.writeRep rep

addScope ::
  Eq def => def ->
  OR.RefMap (TagParam def) (ExprRef def) ->
  ExprRef def -> Infer def ()
addScope def newScope defRep =
  runDecycleT $ addScopeRecurse def newScope defRep

-- Only call on a "root" ref that has no parents (otherwise the scope
-- which should be an intersection is wrong):
lamWrapRef ::
  Eq def => Guid -> ExprRef def -> def ->
  Expr.Kind -> ExprRef def -> Infer def (ExprRef def)
lamWrapRef paramId paramTypeRef def k defRef = do
  defRep <- InferM.liftUFExprs $ UFData.find defRef
  let lam = Expr.Lam k paramId paramTypeRef defRep
  paramIdRep <- InferM.liftGuidAliases $ GuidAliases.getRep paramId
  lamRep <- InferM.liftUFExprs . RefData.fresh emptyScope $ Expr.BodyLam lam
  addScope def (OR.refMapSingleton paramIdRep paramTypeRef) defRep
  return lamRep
  where
    emptyScope = RefData.emptyScope def

lambdaWrap ::
  Eq def =>
  Guid -> ExprRef def ->
  Expr.Expression (LoadedDef def) (ScopedTypedValue def, a) ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue def, Maybe a))
lambdaWrap paramId paramTypeRef expr = do
  typeRef <- InferM.liftUFExprs $ RefData.fresh rootScope $ ExprLens.bodyType # ()
  let
    paramTypeSTVExpr =
      mkRootExpr (ExprLens.bodyHole # ()) $ TypedValue paramTypeRef typeRef
    wrap = lamWrapRef paramId paramTypeRef rootDef
  lambdaRef <- wrap Expr.KVal rootValRef
  piRef <- wrap Expr.KType rootTypRef
  return .
    mkRootExpr (ExprUtil.makeLam Expr.KVal paramId paramTypeSTVExpr (expr <&> Lens._2 %~ Just)) $
    TypedValue lambdaRef piRef
  where
    mkRootExpr body tv =
      Expr.Expression body (ScopedTypedValue tv rootScope, Nothing)
    rootDef =
      case rootScope of
        RefData.Scope _ Nothing ->
          error "Cannot lamWrap: Given a non-root of definition"
        RefData.Scope scopMap _ | not $ OR.refMapNull scopMap ->
          error "Cannot lamWrap: Root of definition has elements in scope?!"
        RefData.Scope _ (Just def) -> def
    ScopedTypedValue (TypedValue rootValRef rootTypRef) rootScope =
      expr ^. Expr.ePayload . Lens._1
