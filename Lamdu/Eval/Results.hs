{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
module Lamdu.Eval.Results where

import           Prelude.Compat

import           Control.Lens (Lens')
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Eval.Val (EvalResult, ScopeId)

data EvalResults pl =
    EvalResults
    { _erExprValues :: Map pl (Map ScopeId (EvalResult ()))
    , _erAppliesOfLam :: Map pl (Map ScopeId [(ScopeId, EvalResult ())])
    } deriving Show

erExprValues :: Lens' (EvalResults pl) (Map pl (Map ScopeId (EvalResult ())))
erExprValues f EvalResults{..} = f _erExprValues <&> \_erExprValues -> EvalResults{..}

erAppliesOfLam :: Lens' (EvalResults pl) (Map pl (Map ScopeId [(ScopeId, EvalResult ())]))
erAppliesOfLam f EvalResults{..} = f _erAppliesOfLam <&> \_erAppliesOfLam -> EvalResults{..}

instance Ord pl => Monoid (EvalResults pl) where
    mempty =
        EvalResults
        { _erExprValues = Map.empty
        , _erAppliesOfLam = Map.empty
        }
    mappend x y =
        EvalResults
        { _erExprValues = Map.unionWith mappend (x ^. erExprValues) (y ^. erExprValues)
        , _erAppliesOfLam =
            Map.unionWith (Map.unionWith (++))
            (x ^. erAppliesOfLam) (y ^. erAppliesOfLam)
        }
