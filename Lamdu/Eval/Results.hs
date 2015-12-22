{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, RecordWildCards #-}
module Lamdu.Eval.Results where

import qualified Control.Lens as Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Eval.Val (EvalResult, ScopeId)

import           Prelude.Compat

data EvalResults pl =
    EvalResults
    { _erExprValues :: Map pl (Map ScopeId (EvalResult ()))
    , _erAppliesOfLam :: Map pl (Map ScopeId [(ScopeId, EvalResult ())])
    } deriving Show

Lens.makeLenses ''EvalResults

empty :: EvalResults pl
empty =
    EvalResults
    { _erExprValues = Map.empty
    , _erAppliesOfLam = Map.empty
    }
