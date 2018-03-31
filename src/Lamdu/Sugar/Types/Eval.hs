-- | Sugared evaluation results
module Lamdu.Sugar.Types.Eval
    ( EvalScopes
    , ChildScopes, ParamScopes, EvaluationScopes, ScopeId
    ) where

import           Data.CurAndPrev (CurAndPrev)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (BinderParamScopeId)
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Eval.Results as ER

import           Lamdu.Prelude

-- TODO: Actually sugar the eval results

type EvalScopes a = CurAndPrev (Map ScopeId a)

-- This is a mapping from a parent scope to the inner scope in:
-- * A redex lambda body (executed exactly once)
-- * Also used for if-else sugar where else-if scopes are executed no more than once
type ChildScopes = EvalScopes ScopeId

type ParamScopes = EvalScopes [BinderParamScopeId]

-- TODO: Is the `Maybe` here relevant? Is an empty map the same?
-- Remove `Maybe` or document the difference
type EvaluationScopes = CurAndPrev (Maybe (Map ScopeId (ER.Val T.Type)))
