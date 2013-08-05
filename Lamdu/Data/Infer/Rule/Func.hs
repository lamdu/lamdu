module Lamdu.Data.Infer.Rule.Func
  ( RuleResult(..), RuleFunc
  ) where

import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (TagExpr)
import Lamdu.Data.Infer.Rule.Types
import Lamdu.Data.Infer.Trigger.Types (Fired)
import qualified Data.OpaqueRef as OR

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

type RuleFunc def = OR.RefMap (TagExpr def) [Fired def] -> Infer def (RuleResult def)
