module Lamdu.Data.Infer.Rule.GetField
  ( make, phase0, phase1, phase2
  ) where

import Control.Lens.Operators
import Control.Monad (void, unless, when)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Rule.Utils (RuleResult(..), RuleFunc, newRule, updateRuleTriggers, liftInfer, ruleDelete)
import Lamdu.Data.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Trigger as Trigger

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

-- Phase0: Verify record has record type:
phase0 :: Eq def => GetFieldPhase0 def -> RuleFunc def
phase0 rule triggers =
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
phase1 :: Eq def => GetFieldPhase1 def -> RuleFunc def
phase1 rule triggers =
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

-- Phase2: Find relevant record fields by triggered tags
phase2 :: Eq def => GetFieldPhase2 def -> RuleFunc def
phase2 =
  updateRuleTriggers RuleGetFieldPhase2 handleTrigger
  where
    handleTrigger (_, False) = return ()
    handleTrigger ((ref, _), True) = do
      rep <- liftInfer . InferM.liftUFExprs $ UFData.find "phase2.ref" ref
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

make :: ExprRef def -> ExprRef def -> ExprRef def -> Infer def ()
make tagValRef getFieldTypeRef recordTypeRef = do
  ruleRef <-
    newRule $
    RuleGetFieldPhase0 GetFieldPhase0
    { _gf0GetFieldTag = tagValRef
    , _gf0GetFieldType = getFieldTypeRef
    }
  Trigger.add Trigger.IsRecordType ruleRef recordTypeRef
