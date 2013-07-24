{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), emptyScope, scopeMap, scopeRefs
  , RenameHistory(..), _Untracked, _RenameHistory
  -- Relations:
  , Relation(..), relationRefs

  , Trigger(..)
  , RefData(..), rdScope, rdRenameHistory, rdRelations, rdBody, rdIsCircumsized, rdTriggers, rdRefs
    , defaultRefData
  , AppliedPiResult(..), aprPiGuid, aprArgVal, aprDestRef, aprCopiedNames, appliedPiResultRefs
  , ExprRefs(..), exprRefsUF, exprRefsData
  , Context(..), ctxExprRefs, ctxDefTVs, ctxRuleMap, ctxRandomGen
    , emptyContext
  , LoadedDef(..), ldDef, ldType
  , TypedValue(..), tvVal, tvType, tvRefs
  , ScopedTypedValue(..), stvTV, stvScope, stvRefs
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref, RefMap)
import Lamdu.Data.Infer.Rule.Internal (RuleIdMap, RuleMap, initialRuleMap)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified System.Random as Random

newtype Scope = Scope (Map Guid Ref) -- intersected
Lens.makeIso ''Scope

emptyScope :: Scope
emptyScope = Scope mempty

scopeMap :: Lens.Iso' Scope (Map Guid Ref)
scopeMap = Lens.from scope

scopeRefs :: Lens.Traversal' Scope Ref
scopeRefs = scopeMap . Lens.traverse

-- Represents a relationship between some subexpression of a Pi result
-- type and the respective sub-expression of an apply type that it
-- should be copied with substs (or unified) into
data AppliedPiResult = AppliedPiResult
  { -- Guid to subst
    _aprPiGuid :: Guid
  , -- Arg val to subst with
    _aprArgVal :: Ref
  , -- Dest Ref
    _aprDestRef :: Ref
  , -- For each src (pi result) guid, remember the dest (apply type)
    -- guid it was copied as and the Ref of the dest param type
    _aprCopiedNames :: Map Guid Guid
  }
Lens.makeLenses ''AppliedPiResult

appliedPiResultRefs :: Lens.Traversal' AppliedPiResult Ref
appliedPiResultRefs f (AppliedPiResult guid argVal destRef copiedNames) =
  AppliedPiResult guid <$> f argVal <*> f destRef <*> pure copiedNames

-- Rename history is only tracked if we're a subst dest (inside an
-- apply type). Then we remember any rename that happened since the
-- subst wrote us.
-- TODO: Do we want fine-grained set of guids to track?
data RenameHistory = Untracked | RenameHistory (Map Guid Guid)
Lens.makePrisms ''RenameHistory

instance Monoid RenameHistory where
  mempty = Untracked
  mappend Untracked x = x
  mappend x Untracked = x
  mappend (RenameHistory m1) (RenameHistory m2) =
    RenameHistory $ mappend m1 m2

-- TODO: Convert to Rules
data Relation = RelationAppliedPiResult AppliedPiResult

relationRefs :: Lens.Traversal' Relation Ref
relationRefs f (RelationAppliedPiResult x) = RelationAppliedPiResult <$> appliedPiResultRefs f x

-- Triggers are alive as long as their truthfulness is yet
-- unknown. Once they're known to be false, they're removed. Once
-- they're known to be true, they trigger a rule and are removed.
data Trigger
  = TriggerIsDirectlyTag
  | TriggerIsRecordType
  deriving (Eq, Ord)

data RefData def = RefData
  { _rdScope :: Scope
  , _rdRenameHistory :: RenameHistory
  , _rdRelations :: [Relation]
  , _rdIsCircumsized :: Monoid.Any
  , _rdTriggers :: RuleIdMap (Set Trigger)
  , _rdBody :: Expr.Body def Ref
  }
Lens.makeLenses ''RefData

defaultRefData :: Scope -> Expr.Body def Ref -> RefData def
defaultRefData scop body = RefData
  { _rdScope = scop
  , _rdRenameHistory = mempty
  , _rdRelations = mempty
  , _rdIsCircumsized = Monoid.Any False
  , _rdTriggers = mempty
  , _rdBody = body
  }

rdRefs :: Lens.Traversal' (RefData def) Ref
rdRefs f (RefData scop renameHistory relations isCircumsized triggers body) =
  RefData
  <$> scopeRefs f scop
  <*> pure renameHistory
  <*> (Lens.traverse . relationRefs) f relations
  <*> pure isCircumsized
  <*> pure triggers
  <*> Lens.traverse f body

data ExprRefs def = ExprRefs
  { _exprRefsUF :: UF.UnionFind
  , _exprRefsData :: RefMap (RefData def)
  }
Lens.makeLenses ''ExprRefs

-- TypedValue:
data TypedValue = TypedValue
  { _tvVal :: {-# UNPACK #-}! Ref
  , _tvType :: {-# UNPACK #-}! Ref
  } deriving (Eq, Ord)
Lens.makeLenses ''TypedValue
instance Show TypedValue where
  showsPrec n (TypedValue v t) =
    showParen (n > 0) (unwords [show v, ":", show t] ++)

tvRefs :: Lens.Traversal' TypedValue Ref
tvRefs f (TypedValue val typ) = TypedValue <$> f val <*> f typ

-- ScopedTypedValue
data ScopedTypedValue = ScopedTypedValue
  { _stvTV :: TypedValue
  , _stvScope :: Scope
  }
Lens.makeLenses ''ScopedTypedValue

stvRefs :: Lens.Traversal' ScopedTypedValue Ref
stvRefs f (ScopedTypedValue tv scop) = ScopedTypedValue <$> tvRefs f tv <*> scopeRefs f scop

-- Context
data Context def = Context
  { _ctxExprRefs :: ExprRefs def
  , _ctxRuleMap :: RuleMap
  , -- NOTE: This Map is for 2 purposes: Sharing Refs of loaded Defs
    -- and allowing to specify recursive defs
    _ctxDefTVs :: Map def TypedValue
  , _ctxRandomGen :: Random.StdGen -- for guids
  }
Lens.makeLenses ''Context

emptyContext :: Random.StdGen -> Context def
emptyContext gen =
  Context
  { _ctxExprRefs =
    ExprRefs
    { _exprRefsUF = UF.empty
    , _exprRefsData = mempty
    }
  , _ctxRuleMap = initialRuleMap
  , _ctxDefTVs = Map.empty
  , _ctxRandomGen = gen
  }

data LoadedDef def = LoadedDef
  { _ldDef :: def
  , _ldType :: Ref
  }
Lens.makeLenses ''LoadedDef
