{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Internal
  ( Error(..)
  , RefVars(..)
  , RefData(..), rdVars, rdBody
  , GuidAliases(..), gaUF, gaMap
  , ExprRefs(..), exprRefsUF, exprRefsData
  , Context(..), ctxExprRefs, ctxGuidAliases
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref, RefMap)
import qualified Control.Lens as Lens
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr

-- TODO: Differing ref types
data Error def = VarEscapesScope | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)

data RefVars = RefVars
  { _rvScopeTypes :: Map Guid Ref -- intersected
  , _rvGetVars :: Set Guid -- unified
  }

data RefData def = RefData
  { _rdVars :: RefVars
  , _rdBody :: Expr.Body def Ref
  }
Lens.makeLenses ''RefData

data GuidAliases = GuidAliases
  { _gaUF :: UF.UnionFind
  , _gaMap :: Map Guid Ref
  }
Lens.makeLenses ''GuidAliases

data ExprRefs def = ExprRefs
  { _exprRefsUF :: UF.UnionFind
  , _exprRefsData :: RefMap (RefData def)
  }
Lens.makeLenses ''ExprRefs

data Context def = Context
  { _ctxExprRefs :: ExprRefs def
  , _ctxGuidAliases :: GuidAliases
  }
Lens.makeLenses ''Context
