{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), scopeMap
  , RefData(..), rdScope, rdSubsts, rdMRenameHistory, rdBody
  , Subst(..), sPiGuid, sArgVal, sCopiedRefs, sCopiedNames
  , ExprRefs(..), exprRefsUF, exprRefsData
  , Context(..), ctxExprRefs, ctxDefRefs, ctxRandomGen
  , LoadedDef(..), ldDef, ldType
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref, RefMap)
import qualified Control.Lens as Lens
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified System.Random as Random

newtype Scope = Scope (Map Guid Ref) -- intersected
  deriving (Monoid)
Lens.makeIso ''Scope

scopeMap :: Lens.Iso' Scope (Map Guid Ref)
scopeMap = Lens.from scope

-- Represents a relationship between some subexpression of a Pi result
-- type and the respective sub-expression of an apply type that it
-- should be copied(with substs) into
data Subst = Subst
  { -- Guid to subst
    _sPiGuid :: Guid
  , -- Arg val to subst with
    _sArgVal :: Ref
  -- For cycle detection in dest, remember which parents in src
  -- context were copied into which parents in dest context:
  , _sCopiedRefs :: Map {-src-}Ref {-dest-}Ref
  , -- For each new name guid, remember the old name it was copied as
    -- (this is the name its known by in the apply context):
    _sCopiedNames :: Map Guid Guid
  }
Lens.makeLenses ''Subst

data RefData def = RefData
  { _rdScope :: Scope
  , _rdSubsts :: [Subst]
  , -- Rename history is only active(Just) if we're a subst dest
    -- (inside an apply type). Then we remember any rename that
    -- happened since the subst wrote us.
    _rdMRenameHistory :: Maybe (Map Guid Guid)
  , _rdBody :: Expr.Body def Ref
  }
Lens.makeLenses ''RefData

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
