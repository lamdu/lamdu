module Lamdu.Data.Infer.Rule.Monad
  ( RM, run, liftInfer, ruleDelete
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), execStateT)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Func (RuleResult(..))
import Lamdu.Data.Infer.Rule.Types (RuleContent)
import Lamdu.Data.Infer.Trigger (Trigger)
import qualified Data.Map as Map

type RM rule def = StateT rule (MaybeT (Infer def))

run ::
  (rule -> RuleContent def) ->
  (((ExprRef def, Trigger def), Bool) -> RM rule def ()) ->
  rule -> Map (ExprRef def, Trigger def) Bool ->
  Infer def (RuleResult def)
run mkContent handleTrigger initialRule =
  fmap (maybe RuleDelete (RuleChange . mkContent)) .
  runMaybeT .
  (`execStateT` initialRule) .
  traverse_ handleTrigger . Map.toList

liftInfer :: Infer def a -> RM rule def a
liftInfer = lift . lift

ruleDelete :: RM rule def ()
ruleDelete = lift mzero
