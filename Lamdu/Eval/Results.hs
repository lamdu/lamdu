{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, RecordWildCards #-}
module Lamdu.Eval.Results where

import qualified Control.Lens as Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Eval.Val (EvalResult, ScopeId)

import           Prelude.Compat

data EvalResults srcId =
    EvalResults
    { _erExprValues :: Map srcId (Map ScopeId (EvalResult ()))
    , _erAppliesOfLam :: Map srcId (Map ScopeId [(ScopeId, EvalResult ())])
    } deriving Show

Lens.makeLenses ''EvalResults

empty :: EvalResults srcId
empty =
    EvalResults
    { _erExprValues = Map.empty
    , _erAppliesOfLam = Map.empty
    }
