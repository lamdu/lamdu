{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.RefData
  ( RefData(..), rdScope, rdBody, rdWasNotDirectlyTag, rdTriggers, rdRefs
    , defaultRefData
  , Scope(..), emptyScope, scopeMap, scopeParamRefs, scopeExprRefs
    , scopeNormalize
  , UFExprs
  , fresh, freshHole
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.UnionFind.WithData (UFData)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefTags (TagExpr, ExprRef, ParamRef, TagRule, TagParam)
import Lamdu.Data.Infer.Trigger.Types (Trigger)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases

type UFExprs def = UFData (TagExpr def) (RefData def)

newtype Scope def = Scope (OR.RefMap (TagParam def) (ExprRef def))

emptyScope :: Scope def
emptyScope = Scope mempty

data RefData def = RefData
  { _rdScope :: Scope def
  , _rdWasNotDirectlyTag :: Monoid.Any
  , _rdTriggers :: OR.RefMap (TagRule def) (Set (Trigger def))
  , _rdBody :: Expr.Body def (ExprRef def)
  }
Lens.makeLenses ''RefData

defaultRefData :: Scope def -> Expr.Body def (ExprRef def) -> RefData def
defaultRefData scop body = RefData
  { _rdScope = scop
  , _rdWasNotDirectlyTag = Monoid.Any False
  , _rdTriggers = mempty
  , _rdBody = body
  }

Lens.makeIso ''Scope

scopeMap :: Lens.Iso' (Scope def) (OR.RefMap (TagParam def) (ExprRef def))
scopeMap = Lens.from scope

scopeParamRefs :: Lens.Traversal' (Scope def) (ParamRef def)
scopeParamRefs = scopeMap . OR.unsafeRefMapItems . Lens._1

scopeExprRefs :: Lens.Traversal' (Scope def) (ExprRef def)
scopeExprRefs = scopeMap . Lens.traverse

scopeNormalize :: MonadA m => Scope def -> StateT (GuidAliases def) m (Scope def)
scopeNormalize = scopeParamRefs %%~ GuidAliases.find

rdRefs :: Lens.Traversal' (RefData def) (ExprRef def)
rdRefs f (RefData scop isCircumsized triggers body) =
  RefData
  <$> scopeExprRefs f scop
  <*> pure isCircumsized
  <*> pure triggers
  <*> Lens.traverse f body

fresh :: MonadA m => Scope def -> Expr.Body def (ExprRef def) -> StateT (UFExprs def) m (ExprRef def)
fresh scop body = UFData.fresh $ defaultRefData scop body

freshHole :: MonadA m => Scope def -> StateT (UFExprs def) m (ExprRef def)
freshHole scop = fresh scop $ ExprLens.bodyHole # ()
