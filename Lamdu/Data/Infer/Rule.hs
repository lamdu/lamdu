{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule
  ( execute, makeGetField
  ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind as UF
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Trigger as Trigger

ruleLens :: RuleRef def -> Lens' (Context def) (Maybe (Rule def))
ruleLens ruleId = ctxRuleMap . rmMap . Lens.at ruleId

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

type RuleFunc def = Map (ExprRef def, Trigger) Bool -> Infer def (RuleResult def)

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

handlePotentialMatches :: Eq def => r -> [(ExprRef def, ExprRef def)] -> ExprRef def -> ExprRef def -> Infer def r -> Infer def r
handlePotentialMatches finish fields tag typ other =
  case fields of
  [] -> InferM.error InferM.GetMissingField
  [(fieldTag, fieldTyp)] -> do
    void $ unify fieldTag tag
    void $ unify fieldTyp typ
    return finish
  _ -> other

assertTag :: MonadA m => ExprRef def -> StateT (Context def) m Guid
assertTag ref =
  Lens.zoom ctxUFExprs $ UFData.read ref
  <&> (^? rdBody . ExprLens.bodyTag)
  <&> unsafeUnjust "isTag && not tag?!"

-- Phase0: Verify record has record type:
getFieldPhase0 :: Eq def => GetFieldPhase0 def -> RuleFunc def
getFieldPhase0 rule triggers =
  case Map.toList triggers of
  [((recordTypeRef, _), isRecord)]
    | isRecord -> do
      recordFields <- InferM.liftContext $ assertRecordTypeFields recordTypeRef
      handlePotentialMatches RuleDelete recordFields
        (rule ^. gf0GetFieldTag)
        (rule ^. gf0GetFieldType) $ do
          phase1RuleRef <-
            InferM.liftContext . Lens.zoom ctxRuleMap . new $
            RuleGetFieldPhase1 GetFieldPhase1
            { _gf1GetFieldRecordTypeFields = recordFields
            , _gf1GetFieldType = rule ^. gf0GetFieldType
            }
          Trigger.add TriggerIsDirectlyTag phase1RuleRef $
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
      InferM.liftContext . Lens.zoom ctxRuleMap . new $
      RuleGetFieldPhase2 GetFieldPhase2
        { _gf2Tag = getFieldTag
        , _gf2TagRef = getFieldTagRef
        , _gf2TypeRef = rule ^. gf1GetFieldType
        , _gf2MaybeMatchers = OR.refMapFromList $ rule ^. gf1GetFieldRecordTypeFields
        }
    rule ^. gf1GetFieldRecordTypeFields
      & Lens.traverseOf_ (Lens.traverse . Lens._1) %%~
        Trigger.add TriggerIsDirectlyTag phase2RuleRef
    return RuleDelete
  _ -> error "Only one trigger before phase 1?!"

-- Phase2: Find relevant record fields by triggered tags
getFieldPhase2 :: Eq def => GetFieldPhase2 def -> RuleFunc def
getFieldPhase2 initialRule =
  go initialRule . Map.toList
  where
    go rule [] = return . RuleChange $ RuleGetFieldPhase2 rule
    go rule ((_, False):xs) = go rule xs
    go rule (((ref, _), True):xs) = do
      rep <- InferM.liftUFExprs $ UFData.find "getFieldPhase2.ref" ref
      fieldTag <- InferM.liftContext $ assertTag rep
      (mFieldTypeRef, newMaybeMatchers) <-
        InferM.liftContext .
        Lens.zoom ctxUFExprs $
        UF.unmaintainedRefMapLookup UFData.find rep `runStateT` (rule ^. gf2MaybeMatchers)
      let
        fieldTypeRef = unsafeUnjust "phase2 triggered by wrong ref!" mFieldTypeRef
        optimizedRule = rule & gf2MaybeMatchers .~ newMaybeMatchers
      if fieldTag == rule ^. gf2Tag
        then do
          void . unify fieldTypeRef $ rule ^. gf2TypeRef
          return . RuleChange $ RuleGetFieldPhase2 optimizedRule
        else do
          let filteredRule = optimizedRule & gf2MaybeMatchers . Lens.at rep .~ Nothing
          handlePotentialMatches RuleDelete
            (OR.refMapToList (filteredRule ^. gf2MaybeMatchers))
            (rule ^. gf2TagRef)
            (rule ^. gf2TypeRef) $
              go filteredRule xs

makeGetField :: ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
makeGetField tagValRef getFieldTypeRef recordTypeRef = do
  ruleId <-
    InferM.liftContext . Lens.zoom ctxRuleMap . new $
    RuleGetFieldPhase0 GetFieldPhase0
    { _gf0GetFieldTag = tagValRef
    , _gf0GetFieldType = getFieldTypeRef
    }
  Trigger.add TriggerIsRecordType ruleId recordTypeRef

execute :: Eq def => RuleRef def -> Map (ExprRef def, Trigger) Bool -> Infer def Bool
execute ruleId triggers = do
  mOldRule <- InferM.liftContext $ Lens.use (ruleLens ruleId)
  let Rule ruleTriggerRefs oldRule = unsafeUnjust ("Execute called on bad rule id: " ++ show ruleId) mOldRule
  ruleRes <- ruleRunner oldRule triggers
  InferM.liftContext $
    case ruleRes of
    RuleKeep -> return True
    RuleDelete -> do
      let
        deleteRuleFrom ref =
          UFData.modify ref $ rdTriggers . Lens.at ruleId .~ Nothing
      Lens.zoom ctxUFExprs . traverse_ deleteRuleFrom $ OR.refSetToList ruleTriggerRefs
      ruleLens ruleId .= Nothing
      return False
    RuleChange changed -> do
      ruleLens ruleId .= Just (Rule ruleTriggerRefs changed)
      return True

ruleRunner :: Eq def => RuleContent def -> RuleFunc def
ruleRunner RuleVerifyTag = verifyTag
ruleRunner (RuleGetFieldPhase0 x) = getFieldPhase0 x
ruleRunner (RuleGetFieldPhase1 x) = getFieldPhase1 x
ruleRunner (RuleGetFieldPhase2 x) = getFieldPhase2 x
