module Lamdu.Data.Infer.Rule.Monad
  ( RM, run, liftInfer, ruleDelete
  ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), execStateT)
import Data.Foldable (traverse_)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Func (RuleFunc, RuleResult(..), flatten)
import Lamdu.Data.Infer.Rule.Types (RuleContent)
import Lamdu.Data.Infer.Trigger (Fired)

type RM rule def = StateT rule (MaybeT (Infer def))

run ::
  (rule -> RuleContent def) ->
  ((ExprRef def, Fired def) -> RM rule def ()) ->
  rule -> RuleFunc def
run mkContent handleFires initialRule =
  fmap (maybe RuleDelete (RuleChange . mkContent)) .
  runMaybeT .
  (`execStateT` initialRule) .
  traverse_ handleFires . flatten

liftInfer :: Infer def a -> RM rule def a
liftInfer = lift . lift

ruleDelete :: RM rule def ()
ruleDelete = lift mzero
