{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule
  ( execute
  , makeGetField
  , makeApply
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void, unless, when, mzero)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), execStateT)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_, sequenceA_)
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef, ParamRef)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Trigger (Trigger)
import Lamdu.Data.Infer.Unify (unify, forceLam)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Trigger as Trigger

ruleLens :: RuleRef def -> Lens' (Context def) (Maybe (Rule def))
ruleLens ruleRef = ctxRuleMap . rmMap . Lens.at ruleRef

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

type RuleFunc def = Map (ExprRef def, Trigger def) Bool -> Infer def (RuleResult def)

verifyTag :: RuleFunc def
verifyTag triggers =
  case mViolation of
  Nothing ->
    -- The verifyTag rule is shared amongst everyone, mustn't delete
    -- it!
    return RuleKeep
  Just ref -> InferM.error $ InferM.CompositeTag ref
  where
    mViolation = Map.keys (Map.filter not triggers) ^? Lens.traverse . Lens._1

assertRecordTypeFields :: MonadA m => ExprRef def -> StateT (Context def) m [(ExprRef def, ExprRef def)]
assertRecordTypeFields ref =
  Lens.zoom ctxUFExprs $ UFData.read ref
  <&> (^? rdBody . ExprLens.bodyKindedRecordFields Expr.KType)
  <&> unsafeUnjust "isRecord && not record?!"

handlePotentialMatches ::
  Eq def => [(ExprRef def, ExprRef def)] -> ExprRef def -> ExprRef def ->
  Infer def Bool
handlePotentialMatches fields tag typ =
  case fields of
  [] -> InferM.error InferM.GetMissingField
  [(fieldTag, fieldTyp)] -> do
    void $ unify fieldTag tag
    void $ unify fieldTyp typ
    return True
  _ ->
    return False

assertTag :: MonadA m => ExprRef def -> StateT (Context def) m Guid
assertTag ref =
  Lens.zoom ctxUFExprs $ UFData.read ref
  <&> (^? rdBody . ExprLens.bodyTag)
  <&> unsafeUnjust "isTag && not tag?!"

newRule :: RuleContent def -> Infer def (RuleRef def)
newRule = InferM.liftContext . Lens.zoom ctxRuleMap . new

-- Phase0: Verify record has record type:
getFieldPhase0 :: Eq def => GetFieldPhase0 def -> RuleFunc def
getFieldPhase0 rule triggers =
  case Map.toList triggers of
  [((recordTypeRef, _), isRecord)]
    | isRecord -> do
      recordFields <- InferM.liftContext $ assertRecordTypeFields recordTypeRef
      isFinished <-
        handlePotentialMatches recordFields
        (rule ^. gf0GetFieldTag)
        (rule ^. gf0GetFieldType)
      unless isFinished $ do
        phase1RuleRef <-
          newRule $
          RuleGetFieldPhase1 GetFieldPhase1
          { _gf1GetFieldRecordTypeFields = recordFields
          , _gf1GetFieldType = rule ^. gf0GetFieldType
          }
        Trigger.add Trigger.IsDirectlyTag phase1RuleRef $
          rule ^. gf0GetFieldTag
      return RuleDelete
    | otherwise -> InferM.error InferM.GetFieldRequiresRecord
  _ -> error "A singleton trigger must be used with GetFieldPhase0 rule"

-- Phase1: Get GetField's tag
getFieldPhase1 :: Eq def => GetFieldPhase1 def -> RuleFunc def
getFieldPhase1 rule triggers =
  case Map.toList triggers of
  [(_, False)] -> return RuleDelete -- Not a tag in that position, do nothing
  [((getFieldTagRef, _), True)] -> do
    getFieldTag <- InferM.liftContext $ assertTag getFieldTagRef
    phase2RuleRef <-
      newRule $
      RuleGetFieldPhase2 GetFieldPhase2
        { _gf2Tag = getFieldTag
        , _gf2TagRef = getFieldTagRef
        , _gf2TypeRef = rule ^. gf1GetFieldType
        , _gf2MaybeMatchers = OR.refMapFromList $ rule ^. gf1GetFieldRecordTypeFields
        }
    rule ^. gf1GetFieldRecordTypeFields
      & Lens.traverseOf_ (Lens.traverse . Lens._1) %%~
        Trigger.add Trigger.IsDirectlyTag phase2RuleRef
    return RuleDelete
  _ -> error "Only one trigger before phase 1?!"

updateRuleTriggers ::
  MonadA m =>
  (rule -> RuleContent def) ->
  (((ExprRef def, Trigger def), Bool) -> StateT rule (MaybeT m) ()) ->
  rule -> Map (ExprRef def, Trigger def) Bool ->
  m (RuleResult def)
updateRuleTriggers mkContent handleTrigger initialRule =
  fmap (maybe RuleDelete (RuleChange . mkContent)) .
  runMaybeT .
  (`execStateT` initialRule) .
  traverse_ handleTrigger . Map.toList

liftInfer :: Infer def a -> StateT rule (MaybeT (Infer def)) a
liftInfer = lift . lift

ruleDelete :: StateT rule (MaybeT (Infer def)) ()
ruleDelete = lift mzero

-- Phase2: Find relevant record fields by triggered tags
getFieldPhase2 :: Eq def => GetFieldPhase2 def -> RuleFunc def
getFieldPhase2 =
  updateRuleTriggers RuleGetFieldPhase2 handleTrigger
  where
    handleTrigger (_, False) = return ()
    handleTrigger ((ref, _), True) = do
      rep <- liftInfer . InferM.liftUFExprs $ UFData.find "getFieldPhase2.ref" ref
      fieldTag <- liftInfer . InferM.liftContext $ assertTag rep
      mFieldTypeRef <- do
        rule <- State.get
        (mFieldTypeRef, newMaybeMatchers) <-
          liftInfer . InferM.liftUFExprs $
          OR.refMapUnmaintainedLookup UFData.find rep `runStateT` (rule ^. gf2MaybeMatchers)
        gf2MaybeMatchers .= newMaybeMatchers
        return mFieldTypeRef
      tag <- Lens.use gf2Tag
      if fieldTag == tag
        then do
          let fieldTypeRef = unsafeUnjust "phase2 triggered by wrong ref!" mFieldTypeRef
          liftInfer . void . unify fieldTypeRef =<< Lens.use gf2TypeRef
          ruleDelete
        else do
          gf2MaybeMatchers . Lens.at rep .= Nothing
          isFinished <- do
            rule <- State.get
            liftInfer $
              handlePotentialMatches (OR.refMapToList (rule ^. gf2MaybeMatchers))
              (rule ^. gf2TagRef)
              (rule ^. gf2TypeRef)
          when isFinished ruleDelete

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

handleApply :: Eq def => RuleRef def -> Apply def -> RuleFunc def
handleApply ruleRef =
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

ruleRunner :: Eq def => RuleContent def -> RuleRef def -> RuleFunc def
ruleRunner RuleVerifyTag _ = verifyTag
ruleRunner (RuleGetFieldPhase0 x) _ = getFieldPhase0 x
ruleRunner (RuleGetFieldPhase1 x) _ = getFieldPhase1 x
ruleRunner (RuleGetFieldPhase2 x) _ = getFieldPhase2 x
ruleRunner (RuleApply x) ruleRef = handleApply ruleRef x

execute :: Eq def => RuleRef def -> Map (ExprRef def, Trigger def) Bool -> Infer def Bool
execute ruleRef triggers = do
  mOldRule <- InferM.liftContext $ Lens.use (ruleLens ruleRef)
  let Rule ruleTriggerRefs oldRule = unsafeUnjust ("Execute called on bad rule id: " ++ show ruleRef) mOldRule
  ruleRes <- ruleRunner oldRule ruleRef triggers
  InferM.liftContext $
    case ruleRes of
    RuleKeep -> return True
    RuleDelete -> do
      let
        deleteRuleFrom ref =
          UFData.modify ref $ rdTriggers . Lens.at ruleRef .~ Nothing
      Lens.zoom ctxUFExprs . traverse_ deleteRuleFrom $ OR.refSetToList ruleTriggerRefs
      ruleLens ruleRef .= Nothing
      return False
    RuleChange changed -> do
      ruleLens ruleRef .= Just (Rule ruleTriggerRefs changed)
      return True

makeGetField :: ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
makeGetField tagValRef getFieldTypeRef recordTypeRef = do
  ruleRef <-
    newRule $
    RuleGetFieldPhase0 GetFieldPhase0
    { _gf0GetFieldTag = tagValRef
    , _gf0GetFieldType = getFieldTypeRef
    }
  Trigger.add Trigger.IsRecordType ruleRef recordTypeRef

addPiResultTriggers :: RuleRef def -> ParamRef def -> ExprRef def -> Infer def ()
addPiResultTriggers ruleRef paramRef srcRef = do
  -- Scope handling should be first so that we do the cheap unify
  -- rather than an expensive copy:
  Trigger.add (Trigger.ScopeHasParameterRef paramRef) ruleRef srcRef
  Trigger.add (Trigger.IsParameterRef paramRef) ruleRef srcRef

makeApply :: Guid -> ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
makeApply piGuid argValRef piResultRef applyTypeRef = do
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
