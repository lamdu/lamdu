{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule
  ( execute
  , makeGetField
  , makeApply
  ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void, unless, when, mzero)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), execStateT)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Trigger (Trigger)
import Lamdu.Data.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind as UF
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
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
          liftInfer . InferM.liftContext .
          Lens.zoom ctxUFExprs $
          UF.unmaintainedRefMapLookup UFData.find rep `runStateT` (rule ^. gf2MaybeMatchers)
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

handleApply :: Apply def -> RuleFunc def
handleApply _apply _triggers = return RuleKeep

ruleRunner :: Eq def => RuleContent def -> RuleFunc def
ruleRunner RuleVerifyTag = verifyTag
ruleRunner (RuleGetFieldPhase0 x) = getFieldPhase0 x
ruleRunner (RuleGetFieldPhase1 x) = getFieldPhase1 x
ruleRunner (RuleGetFieldPhase2 x) = getFieldPhase2 x
ruleRunner (RuleApply x) = handleApply x

execute :: Eq def => RuleRef def -> Map (ExprRef def, Trigger def) Bool -> Infer def Bool
execute ruleRef triggers = do
  mOldRule <- InferM.liftContext $ Lens.use (ruleLens ruleRef)
  let Rule ruleTriggerRefs oldRule = unsafeUnjust ("Execute called on bad rule id: " ++ show ruleRef) mOldRule
  ruleRes <- ruleRunner oldRule triggers
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
  Trigger.add (Trigger.IsParameterRef piGuidRep) ruleRef piResultRef
  Trigger.add (Trigger.ScopeHasParameterRef piGuidRep) ruleRef piResultRef
