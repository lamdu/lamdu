{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule
  ( execute, verifyTagId
  ) where

-- import Lamdu.Data.Infer.Rule.Func ()
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Func (RuleResult(..), RuleFunc)
import Lamdu.Data.Infer.Rule.Types (RuleContent(..), rmMap, Rule(..), RuleRef, verifyTagId)
import Lamdu.Data.Infer.Trigger (Trigger)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Apply as RuleApply
import qualified Lamdu.Data.Infer.Rule.GetField as RuleGetField

ruleLens :: RuleRef def -> Lens' (Context def) (Maybe (Rule def))
ruleLens ruleRef = ctxRuleMap . rmMap . Lens.at ruleRef

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

ruleRunner :: Eq def => RuleContent def -> RuleRef def -> RuleFunc def
ruleRunner RuleVerifyTag _ = verifyTag
ruleRunner (RuleGetFieldPhase0 x) _ = RuleGetField.phase0 x
ruleRunner (RuleGetFieldPhase1 x) _ = RuleGetField.phase1 x
ruleRunner (RuleGetFieldPhase2 x) _ = RuleGetField.phase2 x
ruleRunner (RuleApply x) ruleRef = RuleApply.execute ruleRef x

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
