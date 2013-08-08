{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), emptyScope, scopeMap, scopeParamRefs

  , UFExprs

  , RefData(..), rdScope, rdBody, rdWasNotDirectlyTag, rdTriggers
    , defaultRefData
  , fresh, freshHole
  , Context(..), ctxUFExprs, ctxDefTVs, ctxRuleMap, ctxRandomGen, ctxGuidAliases
    , emptyContext
  , LoadedDef(..), ldDef, ldType
  , TypedValue(..), tvVal, tvType, tvRefs
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Map (Map)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefData
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.Rule.Types (RuleMap, initialRuleMap)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified System.Random as Random

-- TypedValue:
data TypedValue def = TypedValue
  { _tvVal :: {-# UNPACK #-}! (ExprRef def)
  , _tvType :: {-# UNPACK #-}! (ExprRef def)
  } deriving (Eq, Ord)
Lens.makeLenses ''TypedValue
instance Show (TypedValue def) where
  showsPrec n (TypedValue v t) =
    showParen (n > 0) (unwords [show v, ":", show t] ++)

tvRefs :: Lens.Traversal' (TypedValue def) (ExprRef def)
tvRefs f (TypedValue val typ) = TypedValue <$> f val <*> f typ

-- ScopedTypedValue
data ScopedTypedValue def = ScopedTypedValue
  { _stvTV :: TypedValue def
  , _stvScope :: Scope def
  }
Lens.makeLenses ''ScopedTypedValue

-- Context
data Context def = Context
  { _ctxUFExprs :: UFExprs def
  , _ctxRuleMap :: RuleMap def
  , -- NOTE: This Map is for 2 purposes: Sharing Refs of loaded Defs
    -- and allowing to specify recursive defs
    _ctxDefTVs :: Map def (TypedValue def)
  , _ctxRandomGen :: Random.StdGen -- for guids
  , _ctxGuidAliases :: GuidAliases def
  }
Lens.makeLenses ''Context

emptyContext :: Random.StdGen -> Context def
emptyContext gen =
  Context
  { _ctxUFExprs = UFData.empty
  , _ctxRuleMap = initialRuleMap
  , _ctxDefTVs = Map.empty
  , _ctxRandomGen = gen
  , _ctxGuidAliases = GuidAliases.empty
  }

data LoadedDef def = LoadedDef
  { _ldDef :: def
  , _ldType :: ExprRef def
  }
Lens.makeLenses ''LoadedDef
