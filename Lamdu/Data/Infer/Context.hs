{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Context
  ( Context(..), uFExprs, defTVs, ruleMap, randomGen, guidAliases, empty
  ) where

import Data.Map (Map)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefData (UFExprs)
import Lamdu.Data.Infer.Rule.Types (RuleMap, initialRuleMap)
import Lamdu.Data.Infer.TypedValue (TypedValue)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified System.Random as Random

-- Context
data Context def = Context
  { _uFExprs :: UFExprs def
  , _ruleMap :: RuleMap def
  , -- NOTE: This Map is for 2 purposes: Sharing Refs of loaded Defs
    -- and allowing to specify recursive defs
    _defTVs :: Map def (TypedValue def)
  , _randomGen :: Random.StdGen -- for guids
  , _guidAliases :: GuidAliases def
  }
Lens.makeLenses ''Context

empty :: Random.StdGen -> Context def
empty gen =
  Context
  { _uFExprs = UFData.empty
  , _ruleMap = initialRuleMap
  , _defTVs = Map.empty
  , _randomGen = gen
  , _guidAliases = GuidAliases.empty
  }
