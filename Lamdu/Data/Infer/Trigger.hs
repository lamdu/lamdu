module Lamdu.Data.Infer.Trigger
  ( add, updateRefData
  ) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Monad (filterM, when)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import qualified Control.Lens as Lens
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

remember ::
  MonadA m =>
  Ref -> RefData def -> Trigger -> RuleId ->
  StateT (Context def) m ()
remember rep refData trigger ruleId =
  ExprRefs.writeRep rep $ refData
  & rdTriggers . Lens.at ruleId <>~ Just (Set.singleton trigger)

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

handleTrigger :: Ref -> RefData def -> RuleId -> Trigger -> Infer def Bool
handleTrigger rep refData ruleId trigger =
  case checkTrigger refData trigger of
    Nothing -> return True
    Just result -> False <$ InferM.ruleTrigger ruleId rep trigger result

updateRefData :: Ref -> RefData def -> Infer def (RefData def)
updateRefData rep refData =
  refData &
  rdTriggers %%~
  fmap (IntMap.filter (not . Set.null)) .
  Lens.itraverse onTriggers
  where
    onTriggers ruleId =
      fmap Set.fromList .
      filterM (handleTrigger rep refData ruleId) .
      Set.toList

add :: Trigger -> RuleId -> Ref -> Infer def ()
add trigger ruleId ref = do
  rep <- InferM.liftContext $ ExprRefs.find "Trigger.add" ref
  refData <- InferM.liftContext $ ExprRefs.readRep rep
  keep <- handleTrigger rep refData ruleId trigger
  when keep . InferM.liftContext $ remember rep refData trigger ruleId
