module Lamdu.Data.Infer.Rule.Apply
  ( make, execute
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Foldable (sequenceA_, traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA, traverse)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, ParamRef)
import Lamdu.Data.Infer.Rule.Func (RuleFunc)
import Lamdu.Data.Infer.Unify (forceLam)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Monad as RuleMonad
import qualified Lamdu.Data.Infer.Rule.Types as Rule
import qualified Lamdu.Data.Infer.Trigger as Trigger
import qualified Lamdu.Data.Infer.Unify as Unify

unify :: Eq def => ExprRef def -> ExprRef def -> RuleMonad.RM rule def (ExprRef def)
unify x y = RuleMonad.liftInfer $ Unify.unify x y

remapSubstGuid :: Guid -> RuleMonad.RM (Rule.Apply def) def Guid
remapSubstGuid srcGuid = do
  srcRep <- RuleMonad.liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep srcGuid
  mDstRef <-
    Lens.zoom Rule.aLinkedNames $
    OR.refMapUnmaintainedLookup
    (const (lift . InferM.liftGuidAliases . GuidAliases.find)) srcRep
  RuleMonad.liftInfer . InferM.liftGuidAliases $
    State.gets . GuidAliases.guidOfRep =<<
    maybe (return srcRep) GuidAliases.find mDstRef

findRep :: ExprRef def -> RuleMonad.RM rule def (ExprRef def)
findRep = RuleMonad.liftInfer . InferM.liftUFExprs . UFData.find "Apply.findRep"

mFindDestRef :: Eq def => ExprRef def -> RuleMonad.RM (Rule.Apply def) def (Maybe (ExprRef def))
mFindDestRef srcRef =
  try srcRef $ do
    srcRep <- findRep srcRef
    try srcRep $ do
      normalizeSrcLinks
      newSrcRep <- findRep srcRef
      when (newSrcRep /= srcRep) $ error "normalizeSrcLinks can unify srcRep?!"
      try srcRep $ return Nothing
  where
    try ref orElse = do
      mDestRef <- Lens.use (Rule.aLinkedExprs . Lens.at ref)
      case mDestRef of
        Just destRef -> return (Just destRef)
        Nothing -> orElse

normalizeSrcLinks :: Eq def => RuleMonad.RM (Rule.Apply def) def ()
normalizeSrcLinks = do
  linkedPairs <- State.gets (^@.. Rule.aLinkedExprs .> Lens.itraversed)
  normalizedPairs <-
    linkedPairs
    & Lens.traverse . Lens._1 %%~ findRep
    <&> Lens.traverse . Lens._2 %~ (:[])
    <&> OR.refMapFromListWith (++)
  let
    unifyAll [x] = return x
    unifyAll ~(x:xs) = unify x =<< unifyAll xs
  combinedPairs <- traverse unifyAll normalizedPairs
  Rule.aLinkedExprs .= combinedPairs

addPiResultTriggers :: Rule.RuleRef def -> ParamRef def -> ExprRef def -> Infer def ()
addPiResultTriggers ruleRef paramRef srcRef = do
  Trigger.add (Trigger.OnParameterRef paramRef) ruleRef srcRef
  Trigger.add Trigger.OnUnify ruleRef srcRef

link ::
  Eq def =>
  Rule.RuleRef def ->
  ExprRef def -> ExprRef def ->
  RuleMonad.RM (Rule.Apply def) def ()
link ruleRef srcRef dstRef = do
  mOldDestRef <- mFindDestRef srcRef
  case mOldDestRef of
    Just oldDestRef ->
      -- This src is unified with a src that was already copied somewhere:
      void $ unify dstRef oldDestRef
    Nothing -> do
      Rule.aLinkedExprs <>= OR.refMapSingleton srcRef dstRef
      piGuidRep <- RuleMonad.liftInfer . InferM.liftGuidAliases . GuidAliases.getRep =<< Lens.use Rule.aPiGuid
      RuleMonad.liftInfer $ addPiResultTriggers ruleRef piGuidRep srcRef

makePiResultCopy ::
  Eq def =>
  Rule.RuleRef def ->
  ExprRef def -> ExprRef def ->
  RuleMonad.RM (Rule.Apply def) def ()
makePiResultCopy ruleRef srcRef destRef = do
  srcBody <- RuleMonad.liftInfer . InferM.liftUFExprs $ (^. rdBody) <$> UFData.read srcRef
  destScope <- RuleMonad.liftInfer . InferM.liftUFExprs $ (^. rdScope) <$> UFData.read destRef
  case srcBody of
    Expr.BodyLam (Expr.Lam k srcGuid _ _) -> do
      (destGuid, _, _) <- RuleMonad.liftInfer $ forceLam k destScope destRef
      srcNameRep <- RuleMonad.liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep srcGuid
      destNameRep <- RuleMonad.liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep destGuid
      Rule.aLinkedNames . Lens.at srcNameRep .= Just destNameRep
    _ -> do
      destBodyRef <-
        srcBody
        & Lens.traverse %%~ (const . RuleMonad.liftInfer . InferM.liftUFExprs) (freshHole destScope)
        >>= ExprLens.bodyParameterRef %%~ remapSubstGuid
        >>= RuleMonad.liftInfer . InferM.liftUFExprs . fresh destScope
      void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
  destBody <- RuleMonad.liftInfer . InferM.liftUFExprs $ (^. rdBody) <$> UFData.read destRef
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBodyDeprecated matchLamResult (link ruleRef)
    ((const . const) True) srcBody destBody
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    matchLamResult srcGuid _ srcChildRef destChildRef =
      (srcGuid, link ruleRef srcChildRef destChildRef)

execute :: Eq def => Rule.RuleRef def -> Rule.Apply def -> RuleFunc def
execute ruleRef =
  RuleMonad.run Rule.RuleApply handleTrigger
  where
    findDestRef =
      fmap (unsafeUnjust "Trigger.IsParameterRef not on src?!") . mFindDestRef
    handleTrigger (srcRef, Trigger.FiredParameterRef _ Trigger.IsParameterRef) = do
      destRef <- findDestRef srcRef
      argVal <- Lens.use Rule.aArgVal
      void $ unify argVal destRef
    handleTrigger (srcRef, Trigger.FiredParameterRef _ Trigger.NotParameterRef) = do
      -- Triggered when not a hole anymore, so copy:
      mDestRef <- mFindDestRef srcRef
      -- If mDestRef is Nothing, ParameterRefOutOfScope triggered first
      -- and unified instead, so no need to make a copy:
      traverse_ (makePiResultCopy ruleRef srcRef) mDestRef
    handleTrigger (srcRef, Trigger.FiredParameterRef _ Trigger.ParameterRefOutOfScope) = do
      -- Now we know no subexpr can possibly use the piGuid, so it
      -- must fully equal the dest:
      srcRep <- findRep srcRef
      void $ unify srcRep =<< findDestRef srcRep
      -- aLinkedExprs now guaranteed to have the rep:
      Rule.aLinkedExprs . Lens.at srcRep .= Nothing
    handleTrigger (_, Trigger.FiredUnify _) =
      -- Some of our sources were potentially unified, so
      -- normalizeSrcLinks will find them and unify the dests
      normalizeSrcLinks
    handleTrigger trigger = error $ "handleTrigger called with: " ++ show trigger

make :: Guid -> ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
make piGuid argValRef piResultRef applyTypeRef = do
  ruleRef <-
    InferM.liftRuleMap . Rule.new $
    Rule.RuleApply Rule.Apply
    { Rule._aPiGuid = piGuid
    , Rule._aArgVal = argValRef
    , Rule._aLinkedExprs = OR.refMapSingleton piResultRef applyTypeRef
    , Rule._aLinkedNames = OR.refMapEmpty
    }
  piGuidRep <- InferM.liftGuidAliases $ GuidAliases.getRep piGuid
  addPiResultTriggers ruleRef piGuidRep piResultRef
