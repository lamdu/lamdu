module Lamdu.Data.Infer.Trigger
  ( add
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import qualified Control.Lens as Lens
import qualified Data.Set as Set
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
    | refData ^. rdIsCircumsized . Lens.unwrapped
    || Lens.nullOf (rdBody . ExprLens.bodyHole) refData -> Just False
    | otherwise -> Nothing

add :: Ref -> Trigger -> RuleId -> Infer def ()
add ref trigger ruleId = do
  rep <- InferM.liftContext $ ExprRefs.find "Trigger.add" ref
  refData <- InferM.liftContext $ ExprRefs.readRep rep
  case checkTrigger refData trigger of
    Nothing -> -- No information yet:
      InferM.liftContext $ remember rep refData trigger ruleId
    Just result ->
      InferM.ruleTrigger ruleId rep trigger result
