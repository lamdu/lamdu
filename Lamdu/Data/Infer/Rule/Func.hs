module Lamdu.Data.Infer.Rule.Func
  ( RuleResult(..), RuleFunc
  , flatten
  ) where

import Control.Lens.Operators
import Data.Set (Set)
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.RefTags (TagExpr, ExprRef)
import Lamdu.Data.Infer.Rule.Types
import Lamdu.Data.Infer.Trigger.Types (Fired)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Data.Set as Set

data RuleResult def
  = RuleKeep
  | RuleDelete
  | RuleChange (RuleContent def)

flatten :: OR.RefMap (TagExpr def) (Set (Fired def)) -> [(ExprRef def, Fired def)]
flatten =
  concatMap f . (^@.. Lens.itraversed)
  where
    f (x, ys) = map ((,) x) $ Set.toList ys

type RuleFunc def = OR.RefMap (TagExpr def) (Set (Fired def)) -> Infer def (RuleResult def)
