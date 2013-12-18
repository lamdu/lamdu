module Lamdu.Data.Infer.Rule.Apply
  ( make, execute
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Foldable (sequenceA_, traverse_)
import Data.List (partition)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA, traverse)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, ParamRef, TagExpr)
import Lamdu.Data.Infer.Rule.Func (RuleFunc)
import Lamdu.Data.Infer.Unify (forceLam)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.Rule.Monad as RuleMonad
import qualified Lamdu.Data.Infer.Rule.Types as Rule
import qualified Lamdu.Data.Infer.Trigger as Trigger
import qualified Lamdu.Data.Infer.Unify as Unify

unify :: Ord def => ExprRef def -> ExprRef def -> RuleMonad.RM rule def (ExprRef def)
unify x y = RuleMonad.liftInfer $ Unify.unify x y

remapSubstGuid :: Guid -> RuleMonad.RM (Rule.Apply def) def Guid
remapSubstGuid srcGuid = do
  srcRep <- RuleMonad.liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep srcGuid
  (_, mDstRef) <-
    Lens.zoom Rule.aLinkedNames $
    OR.refMapUnmaintainedLookup
    (lift . InferM.liftGuidAliases . GuidAliases.find) srcRep
  RuleMonad.liftInfer . InferM.liftGuidAliases $
    State.gets . GuidAliases.guidOfRep =<<
    maybe (return srcRep) GuidAliases.find mDstRef

findRep :: ExprRef def -> RuleMonad.RM rule def (ExprRef def)
findRep = RuleMonad.liftInfer . InferM.liftUFExprs . UFData.find

mFindLinkBySrc ::
  Ord def => ExprRef def ->
  RuleMonad.RM (Rule.Apply def) def (Maybe (ExprRef def, Rule.ExprLink def))
mFindLinkBySrc srcRef =
  try srcRef $ do
    srcRep <- findRep srcRef
    try srcRep $ do
      normalizeSrcLinks
      newSrcRep <- findRep srcRef
      when (newSrcRep /= srcRep) $ error "normalizeSrcLinks can unify srcRep?!"
      try srcRep $ return Nothing
  where
    try ref orElse = do
      mLinkData <- Lens.use (Rule.aLinkedExprs . Lens.at ref)
      case mLinkData of
        Just linkData -> return $ Just (ref, linkData)
        Nothing -> orElse

normalizeSrcLinks :: Ord def => RuleMonad.RM (Rule.Apply def) def ()
normalizeSrcLinks = do
  linkedPairs <- State.gets (^@.. Rule.aLinkedExprs .> Lens.itraversed)
  normalizedPairs <-
    linkedPairs
    & Lens.traverse . Lens._1 %%~ findRep
    <&> Lens.traverse . Lens._2 %~ (:[])
    <&> OR.refMapFromListWith (++)
  let
    unifyAll [x] = return x
    unifyAll ~(Rule.ExprLink x xAncestors:xs) = do
      Rule.ExprLink y yAncestors <- unifyAll xs
      u <- unify x y
      pure . Rule.ExprLink u $ mappend xAncestors yAncestors
  combinedPairs <- traverse unifyAll normalizedPairs
  Rule.aLinkedExprs .= combinedPairs

addPiResultTriggers ::
  Rule.RuleRef def -> ParamRef def ->
  ExprRef def -> ExprRef def ->
  Infer def ()
addPiResultTriggers ruleRef paramRef srcRef dstRef = do
  Trigger.add [RefData.MustMatch dstRef] (Trigger.OnParameterRef paramRef) ruleRef srcRef -- TODO: Restrictions
  Trigger.add [] Trigger.OnUnify ruleRef srcRef -- TODO: Restrictions

link ::
  Ord def =>
  Rule.RuleRef def ->
  ExprRef def -> ExprRef def ->
  OR.RefSet (TagExpr def) ->
  RuleMonad.RM (Rule.Apply def) def ()
link ruleRef srcRef dstRef dstAncestors = do
  mOldLink <- mFindLinkBySrc srcRef
  case mOldLink of
    Just (linkSrcRef, Rule.ExprLink oldDestRef oldDestAncestors) -> do
      -- This src is unified with a src that was already copied somewhere:
      let
        newLink =
          Just . Rule.ExprLink oldDestRef $
          mappend oldDestAncestors dstAncestors
      Rule.aLinkedExprs . Lens.at linkSrcRef .= newLink
      void $ unify dstRef oldDestRef
    Nothing -> do
      Rule.aLinkedExprs <>= OR.refMapSingleton srcRef (Rule.ExprLink dstRef dstAncestors)
      piGuidRep <- RuleMonad.liftInfer . InferM.liftGuidAliases . GuidAliases.getRep =<< Lens.use Rule.aPiGuid
      RuleMonad.liftInfer $ addPiResultTriggers ruleRef piGuidRep srcRef dstRef

makePiResultCopy ::
  Ord def =>
  Rule.RuleRef def ->
  ExprRef def -> Rule.ExprLink def ->
  RuleMonad.RM (Rule.Apply def) def ()
makePiResultCopy ruleRef srcRef (Rule.ExprLink destRef destAncestors)
  | destAncestors ^. Lens.contains srcRef =
    RuleMonad.liftInfer . InferM.error $ InferM.InfiniteExpression srcRef
  | otherwise = do
  srcBody <- RuleMonad.liftInfer . InferM.liftUFExprs $ (^. RefData.rdBody) <$> UFData.read srcRef
  destScope <- RuleMonad.liftInfer . InferM.liftUFExprs $ (^. RefData.rdScope) <$> UFData.read destRef
  case srcBody of
    Expr.BodyLam (Expr.Lam k srcGuid _ _) -> do
      (destGuid, _, _) <- RuleMonad.liftInfer $ forceLam k destScope destRef
      srcNameRep <- RuleMonad.liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep srcGuid
      destNameRep <- RuleMonad.liftInfer . InferM.liftGuidAliases $ GuidAliases.getRep destGuid
      Rule.aLinkedNames . Lens.at srcNameRep .= Just destNameRep
    _ -> do
      destBodyRef <-
        srcBody
        & Lens.traverse %%~ (const . RuleMonad.liftInfer . InferM.liftContext) (Context.freshHole destScope)
        >>= ExprLens.bodyParameterRef %%~ remapSubstGuid
        >>= RuleMonad.liftInfer . InferM.liftContext . Context.fresh destScope
      void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
  destBody <- RuleMonad.liftInfer . InferM.liftUFExprs $ (^. RefData.rdBody) <$> UFData.read destRef
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBodyDeprecated matchLamResult match
    ((const . const) True) srcBody destBody
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    destChildAncestors = mappend destAncestors $ OR.refSetSingleton destRef
    match srcChildRef destChildRef =
      link ruleRef srcChildRef destChildRef destChildAncestors
    matchLamResult srcGuid _ srcChildRef destChildRef =
      (srcGuid, match srcChildRef destChildRef)

execute :: Ord def => Rule.RuleRef def -> Rule.Apply def -> RuleFunc def
execute ruleRef =
  RuleMonad.run Rule.RuleApply (traverse_ handleTrigger . orderTriggers)
  where
    findLink msg =
      fmap (unsafeUnjust msg) . mFindLinkBySrc
    handleTrigger (srcRef, Trigger.FiredParameterRef _ Trigger.IsTheParameterRef) = do
      (linkSrc, exprLink) <- findLink "Trigger.IsTheParameterRef not on src?!" srcRef
      removeLink linkSrc
      argVal <- Lens.use Rule.aArgVal
      void . unify argVal $ exprLink ^. Rule.dest
    handleTrigger (srcRef, Trigger.FiredParameterRef _ Trigger.NotTheParameterRef) = do
      -- Triggered when not a hole anymore, so copy:
      mLinkPair <- mFindLinkBySrc srcRef
      -- If mDestRef is Nothing, TheParameterOutOfScope triggered first
      -- and unified instead, so no need to make a copy:
      traverse_ (uncurry (makePiResultCopy ruleRef)) mLinkPair
      -- We do not delete the link, because a Trigger.TheParameterOutOfScope may still trigger
    handleTrigger (srcRef, Trigger.FiredParameterRef _ Trigger.TheParameterOutOfScope) = do
      -- Now we know no subexpr can possibly use the piGuid, so it
      -- must fully equal the dest:
      mLink <- mFindLinkBySrc srcRef
      case mLink of
        Nothing ->
          -- If this triggers fires more than once but a unify merges the source of it,
          -- we may have already deleted the link
          return ()
        Just (linkSrc, linkData) -> do
          removeLink linkSrc
          void . unify linkSrc $ linkData ^. Rule.dest
    handleTrigger (_, Trigger.FiredUnify _) =
      -- Some of our sources were potentially unified, so
      -- normalizeSrcLinks will find them and unify the dests
      normalizeSrcLinks
    handleTrigger firings = error $ "handleTrigger called with: " ++ show firings
    removeLink linkSrc =
      Rule.aLinkedExprs . Lens.at linkSrc %= remove
      where
        remove Nothing = error "aLinkedExprs should have the rep"
        remove (Just _) = Nothing
    orderTriggers triggers =
      take 1 unifies ++ others
      where
        (unifies, others) = partition (Lens.has (Lens._2 . Trigger._FiredUnify)) triggers

make :: Guid -> ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
make piGuid argValRef piResultRef applyTypeRef = do
  ruleRef <-
    InferM.liftRuleMap . Rule.new $
    Rule.RuleApply Rule.Apply
    { Rule._aPiGuid = piGuid
    , Rule._aArgVal = argValRef
    , Rule._aLinkedExprs = OR.refMapSingleton piResultRef (Rule.ExprLink applyTypeRef mempty)
    , Rule._aLinkedNames = OR.refMapEmpty
    }
  piGuidRep <- InferM.liftGuidAliases $ GuidAliases.getRep piGuid
  addPiResultTriggers ruleRef piGuidRep piResultRef applyTypeRef
