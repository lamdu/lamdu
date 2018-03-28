-- | Sugared evaluation results

module Lamdu.Sugar.Types.Eval
    ( ChildScopes, ParamScopes, EvaluationScopes, ScopeId
    ) where

import           Data.CurAndPrev (CurAndPrev)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (BinderParamScopeId)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Eval.Results (ScopeId)

import           Lamdu.Prelude

-- TODO: Actually sugar the eval results

-- This is a mapping from a parent scope to the inner scope in:
-- * A redex lambda body (executed exactly once)
-- * Also used for if-else sugar where else-if scopes are executed no more than once
type ChildScopes = CurAndPrev (Map ScopeId ScopeId)

type ParamScopes = CurAndPrev (Map ScopeId [BinderParamScopeId])

-- TODO: Is the `Maybe` here relevant? Is an empty map the same?
-- Remove `Maybe` or document the difference
type EvaluationScopes = CurAndPrev (Maybe (Map ScopeId (ER.Val T.Type)))
