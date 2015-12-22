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

empty :: EvalResults pl
empty =
    EvalResults
    { _erExprValues = Map.empty
    , _erAppliesOfLam = Map.empty
    }
