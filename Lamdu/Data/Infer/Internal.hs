{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), emptyScope, scopeMap
  , RenameHistory(..), _Untracked, _RenameHistory
  -- Relations:
  , GetFieldRefs(..)
  , Relation(..)

  , RefData(..), rdScope, rdAppliedPiResults, rdRenameHistory, rdRelations, rdBody
    , defaultRefData
  , AppliedPiResult(..), aprPiGuid, aprArgVal, aprDestRef, aprCopiedNames
  , ExprRefs(..), exprRefsUF, exprRefsData
  , Context(..), ctxExprRefs, ctxDefRefs, ctxRandomGen
    , emptyContext
  , LoadedDef(..), ldDef, ldType
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref, RefMap)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified System.Random as Random

newtype Scope = Scope (Map Guid Ref) -- intersected
Lens.makeIso ''Scope

emptyScope :: Scope
emptyScope = Scope mempty

scopeMap :: Lens.Iso' Scope (Map Guid Ref)
scopeMap = Lens.from scope

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
    _aprCopiedNames :: Map Guid (Guid, Ref)
  }
Lens.makeLenses ''AppliedPiResult

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

data GetFieldRefs = GetFieldRefs
  { _gfrTag :: Ref
  , _gfrType :: Ref
  , _gfrRecordType :: Ref
  } deriving (Eq, Ord)

data Relation
  = -- Sits in: Record type of get field, get field type, get field
    -- tag, record tags:
    RelationGetField GetFieldRefs
  | RelationIsTag -- Hole | Tag, nothing else
  deriving (Eq, Ord)

data RefData def = RefData
  { _rdScope :: Scope
  , _rdAppliedPiResults :: [AppliedPiResult] -- TODO: Into relations
  , _rdRenameHistory :: RenameHistory
  , _rdRelations :: [Relation]
  , _rdBody :: Expr.Body def Ref
  }
Lens.makeLenses ''RefData

defaultRefData :: Scope -> Expr.Body def Ref -> RefData def
defaultRefData scop body = RefData
  { _rdScope = scop
  , _rdAppliedPiResults = []
  , _rdRenameHistory = mempty
  , _rdRelations = mempty
  , _rdBody = body
  }

data ExprRefs def = ExprRefs
  { _exprRefsUF :: UF.UnionFind
  , _exprRefsData :: RefMap (RefData def)
  }
Lens.makeLenses ''ExprRefs

data Context def = Context
  { _ctxExprRefs :: ExprRefs def
  , -- NOTE: This Map is for 2 purposes: Sharing Refs of loaded Defs
    -- and allowing to specify recursive defs
    _ctxDefRefs :: Map def Ref
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
  , _ctxDefRefs = Map.empty
  , _ctxRandomGen = gen
  }

data LoadedDef def = LoadedDef
  { _ldDef :: def
  , _ldType :: Ref
  }
Lens.makeLenses ''LoadedDef

data TypedValue = TypedValue
  { _tvVal :: {-# UNPACK #-}! Ref
  , _tvType :: {-# UNPACK #-}! Ref
  } deriving (Eq, Ord)
Lens.makeLenses ''TypedValue
instance Show TypedValue where
  show (TypedValue v t) = unwords [show v, ":", show t]

data ScopedTypedValue = ScopedTypedValue
  { _stvTV :: TypedValue
  , _stvScope :: Scope
  }
Lens.makeLenses ''ScopedTypedValue
