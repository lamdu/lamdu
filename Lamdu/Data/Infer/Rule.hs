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
import Data.UnionFind (Ref, unmaintainedRefMapLookup)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Trigger as Trigger

ruleLens :: RuleId -> Lens' (Context def) (Maybe Rule)
ruleLens ruleId = ctxRuleMap . rmMap . Lens.at ruleId

data RuleResult
  = RuleKeep
  | RuleDelete
  | RuleChange RuleContent

type RuleFunc def = Map (Ref, Trigger) Bool -> Infer def RuleResult

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

assertRecordTypeFields :: MonadA m => Ref -> StateT (Context def) m [(Ref, Ref)]
assertRecordTypeFields ref =
  ExprRefs.read ref
  <&> (^? rdBody . ExprLens.bodyKindedRecordFields Expr.KType)
  <&> unsafeUnjust "isRecord && not record?!"

handlePotentialMatches :: Eq def => r -> [(Ref, Ref)] -> Ref -> Ref -> Infer def r -> Infer def r
handlePotentialMatches finish fields tag typ other =
  case fields of
  [] -> InferM.error InferM.GetMissingField
  [(fieldTag, fieldTyp)] -> do
    void $ unify fieldTag tag
    void $ unify fieldTyp typ
    return finish
  _ -> other

assertTag :: MonadA m => Ref -> StateT (Context def) m Guid
assertTag ref =
  ExprRefs.read ref
  <&> (^? rdBody . ExprLens.bodyTag)
  <&> unsafeUnjust "isTag && not tag?!"

-- Phase0: Verify record has record type:
getFieldPhase0 :: Eq def => GetFieldPhase0 -> RuleFunc def
getFieldPhase0 rule triggers =
  case Map.toList triggers of
  [((recordTypeRef, _), isRecord)]
    | isRecord -> do
      recordFields <- InferM.liftContext $ assertRecordTypeFields recordTypeRef
      handlePotentialMatches RuleDelete recordFields
        (rule ^. gf0GetFieldTag)
        (rule ^. gf0GetFieldType) $ do
          phase1RuleId <-
            InferM.liftContext . Lens.zoom ctxRuleMap . new $
            RuleGetFieldPhase1 GetFieldPhase1
            { _gf1GetFieldRecordTypeFields = recordFields
            , _gf1GetFieldType = rule ^. gf0GetFieldType
            }
          Trigger.add TriggerIsDirectlyTag phase1RuleId $
            rule ^. gf0GetFieldTag
          return RuleDelete
    | otherwise -> InferM.error InferM.GetFieldRequiresRecord
  _ -> error "A singleton trigger must be used with GetFieldPhase0 rule"

-- Phase1: Get GetField's tag
getFieldPhase1 :: Eq def => GetFieldPhase1 -> RuleFunc def
getFieldPhase1 rule triggers =
  case Map.toList triggers of
  [(_, False)] -> return RuleDelete -- Not a tag in that position, do nothing
  [((getFieldTagRef, _), True)] -> do
    getFieldTag <- InferM.liftContext $ assertTag getFieldTagRef
    phase2RuleId <-
      InferM.liftContext . Lens.zoom ctxRuleMap . new $
      RuleGetFieldPhase2 GetFieldPhase2
        { _gf2Tag = getFieldTag
        , _gf2TagRef = getFieldTagRef
        , _gf2TypeRef = rule ^. gf1GetFieldType
        , _gf2MaybeMatchers = IntMap.fromList $ rule ^. gf1GetFieldRecordTypeFields
        }
    rule ^. gf1GetFieldRecordTypeFields
      & Lens.traverseOf_ (Lens.traverse . Lens._1) %%~
        Trigger.add TriggerIsDirectlyTag phase2RuleId
    return RuleDelete
  _ -> error "Only one trigger before phase 1?!"

-- Phase2: Find relevant record fields by triggered tags
getFieldPhase2 :: Eq def => GetFieldPhase2 -> RuleFunc def
getFieldPhase2 initialRule =
  go initialRule . Map.toList
  where
    go rule [] = return . RuleChange $ RuleGetFieldPhase2 rule
    go rule ((_, False):xs) = go rule xs
    go rule (((ref, _), True):xs) = do
      rep <- InferM.liftContext $ ExprRefs.find "getFieldPhase2.ref" ref
      fieldTag <- InferM.liftContext $ assertTag rep
      (mFieldTypeRef, newMaybeMatchers) <-
        InferM.liftContext .
        Lens.zoom (ctxExprRefs . exprRefsUF) $
        unmaintainedRefMapLookup rep `runStateT` (rule ^. gf2MaybeMatchers)
      let
        fieldTypeRef = unsafeUnjust "phase2 triggered by wrong ref!" mFieldTypeRef
        optimizedRule = rule & gf2MaybeMatchers .~ newMaybeMatchers
      if fieldTag == rule ^. gf2Tag
        then do
          void . unify fieldTypeRef $ rule ^. gf2TypeRef
          return . RuleChange $ RuleGetFieldPhase2 optimizedRule
        else do
          let filteredRule = optimizedRule & gf2MaybeMatchers %~ IntMap.delete rep
          handlePotentialMatches RuleDelete
            (IntMap.toList (filteredRule ^. gf2MaybeMatchers))
            (rule ^. gf2TagRef)
            (rule ^. gf2TypeRef) $
              go filteredRule xs

makeGetField :: Ref -> Ref -> Ref -> Infer def ()
makeGetField tagValRef getFieldTypeRef recordTypeRef = do
  ruleId <-
    InferM.liftContext . Lens.zoom ctxRuleMap . new $
    RuleGetFieldPhase0 GetFieldPhase0
    { _gf0GetFieldTag = tagValRef
    , _gf0GetFieldType = getFieldTypeRef
    }
  Trigger.add TriggerIsRecordType ruleId $ recordTypeRef

execute :: Eq def => RuleId -> Map (Ref, Trigger) Bool -> Infer def Bool
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
          ExprRefs.modify ref $ rdTriggers %~ IntMap.delete ruleId
      traverse_ deleteRuleFrom $ IntSet.toList ruleTriggerRefs
      ruleLens ruleId .= Nothing
      return False
    RuleChange changed -> do
      ruleLens ruleId .= Just (Rule ruleTriggerRefs changed)
      return True

ruleRunner :: Eq def => RuleContent -> RuleFunc def
ruleRunner RuleVerifyTag = verifyTag
ruleRunner (RuleGetFieldPhase0 x) = getFieldPhase0 x
ruleRunner (RuleGetFieldPhase1 x) = getFieldPhase1 x
ruleRunner (RuleGetFieldPhase2 x) = getFieldPhase2 x
