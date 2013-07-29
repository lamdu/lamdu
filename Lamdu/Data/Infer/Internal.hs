{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), emptyScope, scopeMap, scopeRefs
  , RenameHistory(..), _Untracked, _RenameHistory
  -- Relations:
  , Relation(..), relationRefs

  , ExprRefs, RefD

  , Trigger(..)
  , RefData(..), rdScope, rdRenameHistory, rdRelations, rdBody, rdIsCircumsized, rdTriggers, rdRefs
    , defaultRefData
  , fresh, freshHole
  , AppliedPiResult(..), aprPiGuid, aprArgVal, aprDestRef, aprCopiedNames, appliedPiResultRefs
  , Context(..), ctxExprRefs, ctxDefTVs, ctxRuleMap, ctxRandomGen
    , emptyContext
  , LoadedDef(..), ldDef, ldType
  , TypedValue(..), tvVal, tvType, tvRefs
  , ScopedTypedValue(..), stvTV, stvScope, stvRefs
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.UnionFind.WithData (UFData)
import Lamdu.Data.Infer.RefData
import Lamdu.Data.Infer.Rule.Internal (RuleMap, initialRuleMap)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified System.Random as Random

type ExprRefs def = UFData (RefData def) (RefData def)

-- TypedValue:
data TypedValue def = TypedValue
  { _tvVal :: {-# UNPACK #-}! (RefD def)
  , _tvType :: {-# UNPACK #-}! (RefD def)
  } deriving (Eq, Ord)
Lens.makeLenses ''TypedValue
instance Show (TypedValue def) where
  showsPrec n (TypedValue v t) =
    showParen (n > 0) (unwords [show v, ":", show t] ++)

tvRefs :: Lens.Traversal' (TypedValue def) (RefD def)
tvRefs f (TypedValue val typ) = TypedValue <$> f val <*> f typ

-- ScopedTypedValue
data ScopedTypedValue def = ScopedTypedValue
  { _stvTV :: TypedValue def
  , _stvScope :: Scope def
  }
Lens.makeLenses ''ScopedTypedValue

stvRefs :: Lens.Traversal' (ScopedTypedValue def) (RefD def)
stvRefs f (ScopedTypedValue tv scop) = ScopedTypedValue <$> tvRefs f tv <*> scopeRefs f scop

-- Context
data Context def = Context
  { _ctxExprRefs :: ExprRefs def
  , _ctxRuleMap :: RuleMap (RefData def)
  , -- NOTE: This Map is for 2 purposes: Sharing Refs of loaded Defs
    -- and allowing to specify recursive defs
    _ctxDefTVs :: Map def (TypedValue def)
  , _ctxRandomGen :: Random.StdGen -- for guids
  }
Lens.makeLenses ''Context

emptyContext :: Random.StdGen -> Context def
emptyContext gen =
  Context
  { _ctxExprRefs = UFData.empty
  , _ctxRuleMap = initialRuleMap
  , _ctxDefTVs = Map.empty
  , _ctxRandomGen = gen
  }

data LoadedDef def = LoadedDef
  { _ldDef :: def
  , _ldType :: RefD def
  }
Lens.makeLenses ''LoadedDef

fresh :: MonadA m => Scope def -> Expr.Body def (RefD def) -> StateT (ExprRefs def) m (RefD def)
fresh scop body = UFData.fresh $ defaultRefData scop body

freshHole :: MonadA m => Scope def -> StateT (ExprRefs def) m (RefD def)
freshHole scop = fresh scop $ ExprLens.bodyHole # ()
