module Lamdu.Data.Infer.Rule.Utils
  ( RuleResult(..)
  , RuleFunc, newRule, updateRuleTriggers, liftInfer, ruleDelete
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), execStateT)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Trigger (Trigger)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Infer.Monad as InferM

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

type RuleFunc def =
  Map (ExprRef def, Trigger def) Bool -> Infer def (RuleResult def)

newRule :: RuleContent def -> Infer def (RuleRef def)
newRule = InferM.liftContext . Lens.zoom ctxRuleMap . new

updateRuleTriggers ::
  MonadA m =>
  (rule -> RuleContent def) ->
  (((ExprRef def, Trigger def), Bool) -> StateT rule (MaybeT m) ()) ->
  rule -> Map (ExprRef def, Trigger def) Bool ->
  m (RuleResult def)
updateRuleTriggers mkContent handleTrigger initialRule =
  fmap (maybe RuleDelete (RuleChange . mkContent)) .
  runMaybeT .
  (`execStateT` initialRule) .
  traverse_ handleTrigger . Map.toList

liftInfer :: Infer def a -> StateT rule (MaybeT (Infer def)) a
liftInfer = lift . lift

ruleDelete :: StateT rule (MaybeT (Infer def)) ()
ruleDelete = lift mzero
