{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.RefData
  ( Trigger(..)
  , RefData(..), rdScope, rdRenameHistory, rdRelations, rdBody, rdIsCircumsized, rdTriggers, rdRefs
    , defaultRefData
  , Scope(..), emptyScope, scopeMap, scopeRefs
  , RenameHistory(..), _Untracked, _RenameHistory
  -- Relations:
  , Relation(..), relationRefs

  -- TODO: Remove
  , AppliedPiResult(..), aprPiGuid, aprArgVal, aprDestRef, aprCopiedNames, appliedPiResultRefs
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.RefTags (ExprRef, TagRule)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.OpaqueRef as OR
import qualified Lamdu.Data.Expression as Expr

newtype Scope def = Scope (Map Guid (ExprRef def)) -- intersected

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
    _aprCopiedNames :: Map Guid Guid
  }

appliedPiResultRefs :: Lens.Traversal' (AppliedPiResult def) (ExprRef def)
appliedPiResultRefs f (AppliedPiResult guid argVal destRef copiedNames) =
  AppliedPiResult guid <$> f argVal <*> f destRef <*> pure copiedNames

-- Rename history is only tracked if we're a subst dest (inside an
-- apply type). Then we remember any rename that happened since the
-- subst wrote us.
-- TODO: Do we want fine-grained set of guids to track?
data RenameHistory = Untracked | RenameHistory (Map Guid Guid)

instance Monoid RenameHistory where
  mempty = Untracked
  mappend Untracked x = x
  mappend x Untracked = x
  mappend (RenameHistory m1) (RenameHistory m2) =
    RenameHistory $ mappend m1 m2

-- TODO: Convert to Rules
data Relation def = RelationAppliedPiResult (AppliedPiResult def)

relationRefs :: Lens.Traversal' (Relation def) (ExprRef def)
relationRefs f (RelationAppliedPiResult x) = RelationAppliedPiResult <$> appliedPiResultRefs f x

-- Triggers are alive as long as their truthfulness is yet
-- unknown. Once they're known to be false, they're removed. Once
-- they're known to be true, they trigger a rule and are removed.
data Trigger
  = TriggerIsDirectlyTag
  | TriggerIsRecordType
  deriving (Eq, Ord)

data RefData def = RefData
  { _rdScope :: Scope def
  , _rdRenameHistory :: RenameHistory
  , _rdRelations :: [Relation def]
  , _rdIsCircumsized :: Monoid.Any
  , _rdTriggers :: OR.RefMap (TagRule def) (Set Trigger)
  , _rdBody :: Expr.Body def (ExprRef def)
  }
Lens.makeLenses ''RefData

defaultRefData :: Scope def -> Expr.Body def (ExprRef def) -> RefData def
defaultRefData scop body = RefData
  { _rdScope = scop
  , _rdRenameHistory = mempty
  , _rdRelations = mempty
  , _rdIsCircumsized = Monoid.Any False
  , _rdTriggers = mempty
  , _rdBody = body
  }

Lens.makeIso ''Scope
Lens.makeLenses ''AppliedPiResult
Lens.makePrisms ''RenameHistory

scopeMap :: Lens.Iso' (Scope def) (Map Guid (ExprRef def))
scopeMap = Lens.from scope

scopeRefs :: Lens.Traversal' (Scope def) (ExprRef def)
scopeRefs = scopeMap . Lens.traverse

rdRefs :: Lens.Traversal' (RefData def) (ExprRef def)
rdRefs f (RefData scop renameHistory relations isCircumsized triggers body) =
  RefData
  <$> scopeRefs f scop
  <*> pure renameHistory
  <*> (Lens.traverse . relationRefs) f relations
  <*> pure isCircumsized
  <*> pure triggers
  <*> Lens.traverse f body
