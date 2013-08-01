module Lamdu.Data.Infer.Rule.Apply
  ( make, execute
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..))
import Data.Foldable (sequenceA_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, ParamRef)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Rule.Utils (RuleFunc, newRule, updateRuleTriggers, liftInfer)
import Lamdu.Data.Infer.Unify (unify, forceLam)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Trigger as Trigger

remapSubstGuid :: Guid -> StateT (Apply def) (MaybeT (Infer def)) Guid
remapSubstGuid srcGuid = do
  srcRep <- liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep srcGuid
  mDstRef <-
    Lens.zoom aLinkedNames $
    OR.refMapUnmaintainedLookup
    (const (lift . InferM.liftGuidAliases . GuidAliases.find)) srcRep
  liftInfer . InferM.liftGuidAliases $
    State.gets . GuidAliases.guidOfRep =<<
    maybe (return srcRep) GuidAliases.find mDstRef

link ::
  Eq def =>
  RuleRef def ->
  ExprRef def -> ExprRef def ->
  StateT (Apply def) (MaybeT (Infer def)) ()
link ruleRef srcRef dstRef = do
  aLinkedExprs <>= OR.refMapSingleton srcRef dstRef
  piGuidRep <- liftInfer . InferM.liftGuidAliases . GuidAliases.getRep =<< Lens.use aPiGuid
  liftInfer $ addPiResultTriggers ruleRef piGuidRep srcRef

makePiResultCopy ::
  Eq def =>
  RuleRef def ->
  ExprRef def -> ExprRef def ->
  StateT (Apply def) (MaybeT (Infer def)) ()
makePiResultCopy ruleRef srcRef destRef = do
  srcBody <- liftInfer . InferM.liftUFExprs $ (^. rdBody) <$> UFData.read srcRef
  destScope <- liftInfer . InferM.liftUFExprs $ (^. rdScope) <$> UFData.read destRef
  case srcBody of
    Expr.BodyLam (Expr.Lam k srcGuid _ _) -> do
      (destGuid, _, _) <- liftInfer $ forceLam k destScope destRef
      srcNameRep <- liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep srcGuid
      destNameRep <- liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep destGuid
      aLinkedNames . Lens.at srcNameRep .= Just destNameRep
    _ -> do
      destBodyRef <-
        srcBody
        & Lens.traverse %%~ (const . liftInfer . InferM.liftUFExprs) (freshHole destScope)
        >>= ExprLens.bodyParameterRef %%~ remapSubstGuid
        >>= liftInfer . InferM.liftUFExprs . fresh destScope
      liftInfer . void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
  destBody <- liftInfer . InferM.liftUFExprs $ (^. rdBody) <$> UFData.read destRef
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBodyDeprecated matchLamResult (link ruleRef)
    ((const . const) True) srcBody destBody
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    matchLamResult srcGuid _ srcChildRef destChildRef =
      (srcGuid, link ruleRef srcChildRef destChildRef)

execute :: Eq def => RuleRef def -> Apply def -> RuleFunc def
execute ruleRef =
  updateRuleTriggers RuleApply handleTrigger
  where
    findDestRef =
      fmap (unsafeUnjust "Trigger.IsParameterRef not on src?!") . mFindDestRef
    mFindDestRef srcRef =
      Lens.zoom aLinkedExprs $
      OR.refMapUnmaintainedLookup
      (fmap (lift . InferM.liftUFExprs) . UFData.find) srcRef
    handleTrigger ((srcRef, Trigger.IsParameterRef {}), True) = do
      destRef <- findDestRef srcRef
      argVal <- Lens.use aArgVal
      liftInfer . void $ unify argVal destRef
    handleTrigger ((srcRef, Trigger.IsParameterRef {}), False) = do
      -- Triggered when not a hole anymore, so copy:
      mDestRef <- mFindDestRef srcRef
      case mDestRef of
        Nothing ->
          -- ScopeHasParemeterRef triggered first and unified instead
          return ()
        Just destRef -> makePiResultCopy ruleRef srcRef destRef
    handleTrigger ((_, Trigger.ScopeHasParameterRef {}), True) = error "ScopeHasParameterRef True?!"
    handleTrigger ((srcRef, Trigger.ScopeHasParameterRef {}), False) = do
      -- Now we know no subexpr can possibly use the piGuid, so it
      -- must fully equal the dest:
      srcRep <-
        liftInfer . InferM.liftUFExprs $
        UFData.find "handleApply.srcRef" srcRef
      liftInfer . void . unify srcRep =<< findDestRef srcRep
      -- aLinkedExprs now guaranteed to have the rep:
      aLinkedExprs . Lens.at srcRep .= Nothing
    handleTrigger trigger = error $ "handleTrigger called with: " ++ show trigger

addPiResultTriggers :: RuleRef def -> ParamRef def -> ExprRef def -> Infer def ()
addPiResultTriggers ruleRef paramRef srcRef = do
  -- Scope handling should be first so that we do the cheap unify
  -- rather than an expensive copy:
  Trigger.add (Trigger.ScopeHasParameterRef paramRef) ruleRef srcRef
  Trigger.add (Trigger.IsParameterRef paramRef) ruleRef srcRef

make :: Guid -> ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
make piGuid argValRef piResultRef applyTypeRef = do
  ruleRef <-
    newRule $
    RuleApply Apply
    { _aPiGuid = piGuid
    , _aArgVal = argValRef
    , _aLinkedExprs = OR.refMapSingleton piResultRef applyTypeRef
    , _aLinkedNames = OR.refMapEmpty
    }
  piGuidRep <- InferM.liftGuidAliases $ GuidAliases.getRep piGuid
  addPiResultTriggers ruleRef piGuidRep piResultRef
