module Lamdu.Data.Infer.Trigger
  ( Trigger(..), Fired(..), ParameterRefEvent(..)
  , add, updateRefData
  ) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Lens.Utils (_fromJust)
import Control.Monad (filterM, when)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefData (RefData)
import Lamdu.Data.Infer.RefData (scopeNormalizeParamRefs)
import Lamdu.Data.Infer.RefTags (ExprRef, ParamRef)
import Lamdu.Data.Infer.Rule.Types (RuleRef)
import Lamdu.Data.Infer.Trigger.Types (Trigger(..), Fired(..), ParameterRefEvent(..))
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.Set as Set
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.Rule.Types as Rule

remember ::
  MonadA m =>
  ExprRef def -> RefData def -> Trigger def -> RuleRef def ->
  StateT (Context def) m ()
remember rep refData trigger ruleId = do
  Lens.zoom Context.uFExprs . UFData.writeRep rep $
    refData & RefData.rdTriggers . Lens.at ruleId <>~ Just (Set.singleton trigger)
  Context.ruleMap . Rule.rmMap . Lens.at ruleId .
    _fromJust "Trigger.remember to missing rule" .
    Rule.ruleTriggersIn <>= OR.refSetSingleton rep

checkDirectlyTag :: RefData def -> Maybe (Fired def)
checkDirectlyTag refData
  | Lens.has (RefData.rdBody . ExprLens.bodyTag) refData = Just $ FiredDirectlyTag True
  | refData ^. RefData.rdWasNotDirectlyTag . Lens.unwrapped
  || Lens.nullOf (RefData.rdBody . ExprLens.bodyHole) refData = Just $ FiredDirectlyTag False
  | otherwise = Nothing

checkParameterRef :: ParamRef def -> RefData def -> Infer def (Maybe (Fired def))
checkParameterRef triggerGuidRef refData
  | Lens.nullOf (RefData.rdScope . RefData.scopeParamRefs) refData =
    -- Scope is empty so this cannot be a parameter Ref
    answer triggerGuidRef ParameterRefOutOfScope
  | otherwise = do
    triggerGuidRep <- InferM.liftGuidAliases $ GuidAliases.find triggerGuidRef
    -- Our caller must hand us a normalized scope
    if triggerGuidRep `notElem` (refData ^.. RefData.rdScope . RefData.scopeParamRefs)
      then answer triggerGuidRep ParameterRefOutOfScope
      else
        case refData ^. RefData.rdBody of
        Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) -> do
          guidRep <- InferM.liftGuidAliases $ GuidAliases.getRep guid
          answer triggerGuidRep $
            if triggerGuidRep == guidRep
            then IsParameterRef
            else NotParameterRef
        Expr.BodyLeaf Expr.Hole -> return Nothing
        _ -> answer triggerGuidRep NotParameterRef
  where
    answer ref = return . Just . FiredParameterRef ref

checkKnownBody :: RefData def -> Maybe (Fired def)
checkKnownBody refData
  | Lens.has ExprLens.bodyHole body = Nothing
  | otherwise = Just . FiredKnownBody $ body & ExprLens.bodyDef .~ ()
  where
    body = refData ^. RefData.rdBody

-- | Must be called with RefData with normalized scope
checkTrigger :: RefData def -> Trigger def -> Infer def (Maybe (Fired def))
checkTrigger refData trigger =
  case trigger of
  OnDirectlyTag -> return $ checkDirectlyTag refData
  OnKnownBody -> return $ checkKnownBody refData
  OnParameterRef triggerGuidRef -> checkParameterRef triggerGuidRef refData
  OnUnify -> return Nothing -- unification trigger is handled in unify

-- | Must be called with RefData with normalized scope
handleTrigger :: ExprRef def -> RefData def -> RuleRef def -> Trigger def -> Infer def Bool
handleTrigger rep refData ruleId trigger = do
  mRes <- checkTrigger refData trigger
  case mRes of
    Nothing -> return True
    Just fired -> False <$ InferM.ruleTrigger ruleId rep fired

-- | Must be called with RefData with normalized scope
updateRefData :: ExprRef def -> RefData def -> Infer def (RefData def)
updateRefData rep refData =
  refData &
  RefData.rdTriggers %%~
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
  refDataNorm <- refData & RefData.rdScope %%~ InferM.liftGuidAliases . scopeNormalizeParamRefs
  keep <- handleTrigger rep refDataNorm ruleId trigger
  when keep . InferM.liftContext $ remember rep refData trigger ruleId
