{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Eval.Results where

import           Prelude.Compat

import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Eval.Val (Val, ScopeId)

data EvalResults pl =
    EvalResults
    { erExprValues :: Map pl (Map ScopeId (Val ()))
    , erAppliesOfLam :: Map pl (Map ScopeId [(ScopeId, Val ())])
    } deriving Show

instance Ord pl => Monoid (EvalResults pl) where
    mempty =
        EvalResults
        { erExprValues = Map.empty
        , erAppliesOfLam = Map.empty
        }
    mappend x y =
        EvalResults
        { erExprValues = Map.unionWith mappend (erExprValues x) (erExprValues y)
        , erAppliesOfLam =
            Map.unionWith (Map.unionWith (++))
            (erAppliesOfLam x) (erAppliesOfLam y)
        }
