module Lamdu.Infer.Rule.Uncircumsize
  ( make, execute
  ) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Monad (void)
import Lamdu.Infer.Monad (Infer)
import Lamdu.Infer.RefTags (ExprRef)
import Lamdu.Infer.Rule.Func (RuleResult(..), RuleFunc)
import Lamdu.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Expr as Expr
import qualified Lamdu.Infer.Context as Context
import qualified Lamdu.Infer.Monad as InferM
import qualified Lamdu.Infer.RefData as RefData
import qualified Lamdu.Infer.Rule.Types as Rule
import qualified Lamdu.Infer.Trigger as Trigger

uncircumsize :: Ord def => Rule.Uncircumsize def -> Infer def ()
uncircumsize rule = do
  scope <- UFData.read (rule ^. Rule.uValRef) <&> (^. RefData.rdScope) & InferM.liftUFExprs
  uncircumsizedRef <- InferM.liftContext . Context.fresh scope $ rule ^. Rule.uUncircumsizedBody
  void $ unify (rule ^. Rule.uValRef) uncircumsizedRef

execute :: Ord def => Rule.Uncircumsize def -> RuleFunc def
execute rule triggers =
  case triggers ^@.. Lens.itraversed <. Lens.folded of
  [(_, Trigger.FiredKnownBody knownBody)] ->
    RuleDelete <$
    case knownBody of
    Expr.VLeaf (Expr.VVar Expr.DefinitionRef {}) ->
      uncircumsize rule
    _ -> return () -- Keep circumsized
  list -> error $ "Uncircumsize.execute: Unexpected firings: " ++ show list

make ::
  ExprRef def -> ExprRef def ->
  RefData.LoadedBody def (ExprRef def) ->
  Infer def ()
make valRef applicantValRef uncircumsizedValBody = do
  ruleRef <-
    InferM.liftRuleMap . Rule.new $
    Rule.RuleUncircumsize Rule.Uncircumsize
    { Rule._uValRef = valRef
    , Rule._uApplicantValRef = applicantValRef
    , Rule._uUncircumsizedBody = uncircumsizedValBody
    }
  Trigger.add [] Trigger.OnKnownBody ruleRef applicantValRef
