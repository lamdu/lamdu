module Lamdu.Infer.Rule.Func
  ( RuleResult(..), RuleFunc
  ) where

import Lamdu.Infer.Monad (Infer)
import Lamdu.Infer.RefTags (TagExpr)
import Lamdu.Infer.Rule.Types
import Lamdu.Infer.Trigger.Types (Fired)
import qualified Data.OpaqueRef as OR

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

type RuleFunc def = OR.RefMap (TagExpr def) [Fired def] -> Infer def (RuleResult def)
