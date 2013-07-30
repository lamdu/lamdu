module Lamdu.Data.Infer.Trigger
  ( add, updateRefData
  ) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Lens.Utils (_fromJust)
import Control.Monad (filterM, when)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal (RuleRef)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Data.Set as Set
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule.Internal as Rule

remember ::
  MonadA m =>
  ExprRef def -> RefData def -> Trigger -> RuleRef def ->
  StateT (Context def) m ()
remember rep refData trigger ruleId = do
  Lens.zoom ctxUFExprs . UFData.writeRep rep $
    refData & rdTriggers . Lens.at ruleId <>~ Just (Set.singleton trigger)
  ctxRuleMap . Rule.rmMap . Lens.at ruleId .
    _fromJust "Trigger.remember to missing rule" .
    Rule.ruleTriggersIn <>= OR.refSetSingleton rep

checkTrigger :: RefData def -> Trigger -> Maybe Bool
checkTrigger refData trigger =
  case trigger of
  TriggerIsDirectlyTag
    | Lens.has (rdBody . ExprLens.bodyTag) refData -> Just True
    | refData ^. rdIsCircumsized . Lens.unwrapped -> Just False
    | otherwise -> checkHole
  TriggerIsRecordType
    | Lens.has (rdBody . ExprLens.bodyKindedRecordFields Expr.KType) refData -> Just True
    | otherwise -> checkHole
  where
    checkHole
      | Lens.nullOf (rdBody . ExprLens.bodyHole) refData = Just False
      | otherwise = Nothing

handleTrigger :: ExprRef def -> RefData def -> RuleRef def -> Trigger -> Infer def Bool
handleTrigger rep refData ruleId trigger =
  case checkTrigger refData trigger of
    Nothing -> return True
    Just result -> False <$ InferM.ruleTrigger ruleId rep trigger result

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

add :: Trigger -> RuleRef def -> ExprRef def -> Infer def ()
add trigger ruleId ref = do
  rep <- InferM.liftUFExprs $ UFData.find "Trigger.add" ref
  refData <- InferM.liftUFExprs $ UFData.readRep rep
  keep <- handleTrigger rep refData ruleId trigger
  when keep . InferM.liftContext $ remember rep refData trigger ruleId
