{-# LANGUAGE RankNTypes #-}
module Lamdu.Infer.Rule
  ( execute, verifyTagId
  ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Lamdu.Infer.Context (Context)
import Lamdu.Infer.Monad (Infer)
import Lamdu.Infer.RefTags (TagExpr)
import Lamdu.Infer.Rule.Func (RuleResult(..), RuleFunc)
import Lamdu.Infer.Rule.Types (RuleContent(..), rmMap, Rule(..), RuleRef, verifyTagId)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Infer.Context as Context
import qualified Lamdu.Infer.Monad as InferM
import qualified Lamdu.Infer.RefData as RefData
import qualified Lamdu.Infer.Rule.Apply as RuleApply
import qualified Lamdu.Infer.Rule.GetField as RuleGetField
import qualified Lamdu.Infer.Rule.Uncircumsize as RuleUncircumsize
import qualified Lamdu.Infer.Trigger as Trigger

ruleLens :: RuleRef def -> Lens' (Context def) (Maybe (Rule def))
ruleLens ruleRef = Context.ruleMap . rmMap . Lens.at ruleRef

verifyTag :: RuleFunc def
verifyTag triggers =
  case violations ^? Lens.traverse of
  Nothing ->
    -- The verifyTag rule is shared amongst everyone, mustn't delete
    -- it!
    return RuleKeep
  Just ref -> InferM.error $ InferM.CompositeTag ref
  where
    violations =
      [ ref
      | (ref, Trigger.FiredDirectlyTag False) <- triggers ^@.. Lens.itraversed <. Lens.folded
      ]

ruleRunner :: Ord def => RuleContent def -> RuleRef def -> RuleFunc def
ruleRunner RuleVerifyTag _ = verifyTag
ruleRunner (RuleGetFieldPhase0 x) _ = RuleGetField.phase0 x
ruleRunner (RuleGetFieldPhase1 x) _ = RuleGetField.phase1 x
ruleRunner (RuleGetFieldPhase2 x) _ = RuleGetField.phase2 x
ruleRunner (RuleApply x) ruleRef = RuleApply.execute ruleRef x
ruleRunner (RuleUncircumsize x) _ = RuleUncircumsize.execute x

execute :: Ord def => RuleRef def -> OR.RefMap (TagExpr def) [Trigger.Fired def] -> Infer def Bool
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
          UFData.modify ref $ RefData.rdTriggers . Lens.at ruleRef .~ Nothing
      Lens.zoom Context.ufExprs . traverse_ deleteRuleFrom $ OR.refSetToList ruleTriggerRefs
      ruleLens ruleRef .= Nothing
      return False
    RuleChange changed -> do
      ruleLens ruleRef .= Just (Rule ruleTriggerRefs changed)
      return True
