{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Internal
  ( RefVars(..)
  , RefData(..), rdVars, rdBody
  , ExprRefs(..), exprRefsUF, exprRefsData
  , Context(..), ctxExprRefs
  , LoadedDef(..), ldDef, ldType
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref, RefMap)
import qualified Control.Lens as Lens
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr

data RefVars = RefVars
  { _rvScopeTypes :: Map Guid Ref -- intersected
  , _rvGetVars :: Set Guid -- unified
  }

data RefData def = RefData
  { _rdVars :: RefVars
  , _rdBody :: Expr.Body def Ref
  }
Lens.makeLenses ''RefData

data ExprRefs def = ExprRefs
  { _exprRefsUF :: UF.UnionFind
  , _exprRefsData :: RefMap (RefData def)
  }
Lens.makeLenses ''ExprRefs

newtype Context def = Context
  { _ctxExprRefs :: ExprRefs def
  }
Lens.makeLenses ''Context

data LoadedDef def = LoadedDef
  { _ldDef :: def
  , _ldType :: Ref
  }
Lens.makeLenses ''LoadedDef
