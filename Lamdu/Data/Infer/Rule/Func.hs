module Lamdu.Data.Infer.Rule.Func
  ( Triggered, RuleResult(..), RuleFunc
  ) where

import Data.Map (Map)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Types
import Lamdu.Data.Infer.Trigger (Trigger)

type Triggered def =
  Map (ExprRef def, Trigger def) Bool

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

type RuleFunc def = Triggered def -> Infer def (RuleResult def)
