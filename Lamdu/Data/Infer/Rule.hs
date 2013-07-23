{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule
  ( execute
  ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import qualified Control.Lens as Lens
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
  | RuleChange Rule

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

getFieldFindTags :: GetFieldFindTags -> RuleFunc def
getFieldFindTags rule triggers =
  case Map.toList triggers of
  [((recordTypeRef, _), isRecord)]
    | isRecord -> do
      recordFields <- InferM.liftContext $ assertRecordTypeFields recordTypeRef
      verificationRuleId <-
        error "TODO"
        --InferM.liftContext . Lens.zoom ctxRuleMap . newRule $
        --RuleGetFieldVerifyTag GetFieldVerifyTag
        --{ _gfvtMTag = Nothing
        --, _gfvtTagRef = rule ^. gfftTag
        --, _gfvtGetFieldTypeRef = rule ^. gfftType
        --, _gfvtPossibleMatches = Map.fromList recordFields
        --}
      let tagTrigger ref = Trigger.add ref TriggerIsDirectlyTag verificationRuleId
      traverse_ tagTrigger $ rule ^. gfftTag : map fst recordFields
      return RuleDelete
    | otherwise -> InferM.error InferM.GetFieldRequiresRecord
  _ -> error "A singleton trigger must be used with GetFieldFindTags rule"

execute :: RuleId -> Map (Ref, Trigger) Bool -> Infer def ()
execute ruleId triggers = do
  mOldRule <- InferM.liftContext $ Lens.use (ruleLens ruleId)
  let oldRule = unsafeUnjust ("Execute called on bad rule id: " ++ show ruleId) mOldRule
  ruleRes <-
    case oldRule of
    RuleVerifyTag -> verifyTag triggers
    RuleGetFieldFindTags x -> getFieldFindTags x triggers
  InferM.liftContext $
    case ruleRes of
    RuleKeep -> return ()
    RuleDelete ->
      ruleLens ruleId .= Just
      (error "TODO: This rule should be deleted with all its triggers")
    RuleChange new ->
      ruleLens ruleId .= Just new
