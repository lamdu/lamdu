module Lamdu.Data.Infer.AppliedPiResult
  ( handleAppliedPiResult
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (when, void)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Foldable (sequenceA_)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Unify (unify, forceLam, normalizeScope)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM

-- Remap a Guid from piResult context to the Apply context
remapSubstGuid ::
  MonadA m => AppliedPiResult def -> Guid -> StateT (GuidAliases def) m Guid
remapSubstGuid apr src = do
  srcRep <- GuidAliases.getRep src
  copiedNames <- apr ^. aprCopiedNames & Lens.traverse . Lens.both %%~ GuidAliases.find
  case lookup srcRep copiedNames of
    Nothing -> return src
    Just destRep -> State.gets $ GuidAliases.guidOfRep destRep

-- TODO: This should also substLeafs, and it should also subst getvars that aren't subst
substNode :: Eq def => Expr.Body def (ExprRef def) -> AppliedPiResult def -> Infer def ()
substNode srcBody apr = do
  destData <- InferM.liftUFExprs $ UFData.read destRef
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBodyDeprecated matchLamResult matchOther
    ((const . const) True) srcBody (destData ^. rdBody)
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    destRef = apr ^. aprDestRef
    recurse srcChildRef destChildRef childApr =
      handleAppliedPiResult srcChildRef $
      childApr & aprDestRef .~ destChildRef
    matchLamResult srcGuid _ srcChildRef destChildRef =
      (srcGuid, recurse srcChildRef destChildRef apr)
    matchOther srcChildRef destChildRef =
      recurse srcChildRef destChildRef apr

handleAppliedPiResult :: Eq def => ExprRef def -> AppliedPiResult def -> Infer def ()
handleAppliedPiResult srcRef apr = do
  srcData <- InferM.liftUFExprs $ UFData.read srcRef
  destScope <- InferM.liftUFExprs $ (^. rdScope) <$> UFData.read destRef
  let srcScope = srcData ^. rdScope
  srcScopeNorm <- normalizeScope srcScope
  destScopeNorm <- normalizeScope destScope
  -- Only if the srcScope has any variable available that's not
  -- already available in the destScope could it be a GetVar.
  let isUnify = OR.refMapNull $ OR.refMapDifference srcScopeNorm destScopeNorm
  if isUnify
    then -- It can't be a GetVar, we can merge and pull information
         -- from the apply:
    void $ unify destRef srcRef
    else -- Do the subst:
    case srcData ^. rdBody of
    Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef paramGuid))
      | paramGuid == apr ^. aprPiGuid -> void . unify destRef $ apr ^. aprArgVal
    Expr.BodyLeaf Expr.Hole ->
      srcData & rdRelations %~ (RelationAppliedPiResult apr :)
      & InferM.liftUFExprs . UFData.write srcRef
    srcBody@(Expr.BodyLam (Expr.Lam k srcGuid _ _)) -> do
      (destGuid, _, _) <- forceLam k destScope destRef
      srcRep <- liftGuidAliases $ GuidAliases.getRep srcGuid
      destRep <- liftGuidAliases $ GuidAliases.getRep destGuid
      substNode srcBody (apr & aprCopiedNames %~ ((srcRep, destRep):))
    srcBody -> do
      destBodyRef <-
        srcBody
        & Lens.traverse %%~ const (InferM.liftUFExprs (freshHole destScope))
        >>= ExprLens.bodyParameterRef %%~ liftGuidAliases . remapSubstGuid apr
        >>= InferM.liftUFExprs . fresh destScope
      void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
      substNode srcBody apr
  where
    liftGuidAliases = InferM.liftContext . Lens.zoom ctxGuidAliases
    destRef = apr ^. aprDestRef
