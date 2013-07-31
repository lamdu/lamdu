{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.RefData
  ( RefData(..), rdScope, rdRelations, rdBody, rdIsCircumsized, rdTriggers, rdRefs
    , defaultRefData
  , Scope(..), emptyScope, scopeMap, scopeParamRefs, scopeExprRefs
  -- Relations:
  , Relation(..), relationRefs

  -- TODO: Remove
  , AppliedPiResult(..), aprPiGuid, aprArgVal, aprDestRef, aprCopiedNames, appliedPiResultRefs

  , UFExprs
  , fresh, freshHole
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.UnionFind.WithData (UFData)
import Lamdu.Data.Infer.RefTags (TagExpr, ExprRef, ParamRef, TagRule)
import Lamdu.Data.Infer.Trigger.Internal (Trigger)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens

type UFExprs def = UFData (TagExpr def) (RefData def)

newtype Scope def = Scope [(ParamRef def, ExprRef def)] -- intersected

emptyScope :: Scope def
emptyScope = Scope mempty

-- Represents a relationship between some subexpression of a Pi result
-- type and the respective sub-expression of an apply type that it
-- should be copied with substs (or unified) into
data AppliedPiResult def = AppliedPiResult
  { -- Guid to subst
    _aprPiGuid :: Guid
  , -- Arg val to subst with
    _aprArgVal :: ExprRef def
  , -- Dest Ref
    _aprDestRef :: ExprRef def
  , -- For each src (pi result) guid, remember the dest (apply type)
    -- guid it was copied as and the Ref of the dest param type
    _aprCopiedNames :: [(ParamRef def, ParamRef def)] -- (src, dest)
  }

appliedPiResultRefs :: Lens.Traversal' (AppliedPiResult def) (ExprRef def)
appliedPiResultRefs f (AppliedPiResult guid argVal destRef copiedNames) =
  AppliedPiResult guid <$> f argVal <*> f destRef <*> pure copiedNames

-- TODO: Convert to Rules
data Relation def = RelationAppliedPiResult (AppliedPiResult def)

relationRefs :: Lens.Traversal' (Relation def) (ExprRef def)
relationRefs f (RelationAppliedPiResult x) = RelationAppliedPiResult <$> appliedPiResultRefs f x

data RefData def = RefData
  { _rdScope :: Scope def
  , _rdRelations :: [Relation def]
  , _rdIsCircumsized :: Monoid.Any
  , _rdTriggers :: OR.RefMap (TagRule def) (Set (Trigger def))
  , _rdBody :: Expr.Body def (ExprRef def)
  }
Lens.makeLenses ''RefData

defaultRefData :: Scope def -> Expr.Body def (ExprRef def) -> RefData def
defaultRefData scop body = RefData
  { _rdScope = scop
  , _rdRelations = mempty
  , _rdIsCircumsized = Monoid.Any False
  , _rdTriggers = mempty
  , _rdBody = body
  }

Lens.makeIso ''Scope
Lens.makeLenses ''AppliedPiResult

scopeMap :: Lens.Iso' (Scope def) [(ParamRef def, ExprRef def)]
scopeMap = Lens.from scope

scopeParamRefs :: Lens.Traversal' (Scope def) (ParamRef def)
scopeParamRefs = scopeMap . Lens.traverse . Lens._1

scopeExprRefs :: Lens.Traversal' (Scope def) (ExprRef def)
scopeExprRefs = scopeMap . Lens.traverse . Lens._2

rdRefs :: Lens.Traversal' (RefData def) (ExprRef def)
rdRefs f (RefData scop relations isCircumsized triggers body) =
  RefData
  <$> scopeExprRefs f scop
  <*> (Lens.traverse . relationRefs) f relations
  <*> pure isCircumsized
  <*> pure triggers
  <*> Lens.traverse f body

fresh :: MonadA m => Scope def -> Expr.Body def (ExprRef def) -> StateT (UFExprs def) m (ExprRef def)
fresh scop body = UFData.fresh $ defaultRefData scop body

freshHole :: MonadA m => Scope def -> StateT (UFExprs def) m (ExprRef def)
freshHole scop = fresh scop $ ExprLens.bodyHole # ()
