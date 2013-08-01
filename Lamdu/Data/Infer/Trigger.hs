{-# LANGUAGE PatternGuards #-}
module Lamdu.Data.Infer.Trigger
  ( Trigger(..)
  , add, updateRefData
  ) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Lens.Utils (_fromJust)
import Control.Monad (filterM, when)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefData (scopeNormalize)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal (RuleRef)
import Lamdu.Data.Infer.Trigger.Internal (Trigger(..))
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.Set as Set
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Internal as Rule

remember ::
  MonadA m =>
  ExprRef def -> RefData def -> Trigger def -> RuleRef def ->
  StateT (Context def) m ()
remember rep refData trigger ruleId = do
  Lens.zoom ctxUFExprs . UFData.writeRep rep $
    refData & rdTriggers . Lens.at ruleId <>~ Just (Set.singleton trigger)
  ctxRuleMap . Rule.rmMap . Lens.at ruleId .
    _fromJust "Trigger.remember to missing rule" .
    Rule.ruleTriggersIn <>= OR.refSetSingleton rep

-- | Must be called with RefData with normalized scope
checkTrigger :: RefData def -> Trigger def -> Infer def (Maybe Bool)
checkTrigger refData trigger =
  case trigger of
  IsDirectlyTag
    | Lens.has (rdBody . ExprLens.bodyTag) refData -> yes
    | refData ^. rdIsCircumsized . Lens.unwrapped -> no
    | otherwise -> checkHole
  IsParameterRef triggerGuidRef
    | Just guid <- refData ^? rdBody . ExprLens.bodyParameterRef -> do
      triggerGuidRep <- InferM.liftGuidAliases $ GuidAliases.find triggerGuidRef
      guidRep <- InferM.liftGuidAliases $ GuidAliases.getRep guid
      return . Just $ triggerGuidRep == guidRep
    | otherwise -> checkHole
  IsRecordType
    | Lens.has (rdBody . ExprLens.bodyKindedRecordFields Expr.KType) refData -> yes
    | otherwise -> checkHole
  ScopeHasParameterRef triggerGuidRef
    | Lens.nullOf (rdScope . scopeParamRefs) refData ->
      -- Scope is empty so this cannot be a parameter Ref
      no
    | otherwise -> do
      triggerGuidRep <- InferM.liftGuidAliases $ GuidAliases.find triggerGuidRef
      -- Our caller must hand us a normalized scope
      if triggerGuidRep `elem` (refData ^.. rdScope . scopeParamRefs)
        then unknown -- It may be removed in the future...
        else no
  where
    yes = return $ Just True
    no = return $ Just False
    unknown = return Nothing
    checkHole
      | Lens.nullOf (rdBody . ExprLens.bodyHole) refData = no
      | otherwise = unknown

-- | Must be called with RefData with normalized scope
handleTrigger :: ExprRef def -> RefData def -> RuleRef def -> Trigger def -> Infer def Bool
handleTrigger rep refData ruleId trigger = do
  mRes <- checkTrigger refData trigger
  case mRes of
    Nothing -> return True
    Just result -> False <$ InferM.ruleTrigger ruleId rep trigger result

-- | Must be called with RefData with normalized scope
updateRefData :: ExprRef def -> RefData def -> Infer def (RefData def)
updateRefData rep refData =
  refData &
  rdTriggers %%~
  fmap (OR.refMapFilter (not . Set.null)) .
  Lens.itraverse onTriggers
  where
    onTriggers ruleId =
      fmap Set.fromList .
      filterM (handleTrigger rep refData ruleId) .
      Set.toList

add :: Trigger def -> RuleRef def -> ExprRef def -> Infer def ()
add trigger ruleId ref = do
  rep <- InferM.liftUFExprs $ UFData.find "Trigger.add" ref
  refData <- InferM.liftUFExprs . State.gets $ UFData.readRep rep
  -- TODO: The tests pass even with the un-normalized Scope. Is there
  -- some guarantee that when we're called here, scope is always
  -- already normalized?
  refDataNorm <- refData & rdScope %%~ InferM.liftGuidAliases . scopeNormalize
  keep <- handleTrigger rep refDataNorm ruleId trigger
  when keep . InferM.liftContext $ remember rep refData trigger ruleId
