{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Internal
  ( Scope(..), emptyScope, scopeMap, scopeRefs
  , RenameHistory(..), _Untracked, _RenameHistory
  -- Relations:
  , Relation(..), relationRefs

  , Trigger(..)
  , RuleId, RuleIdMap
  , GetFieldPhase0(..), gf0GetFieldTag, gf0GetFieldType
  , GetFieldPhase1(..), gf1GetFieldRecordTypeFields, gf1GetFieldType
  , GetFieldPhase2(..), gf2Tag, gf2TagRef, gf2TypeRef, gf2MaybeMatchers
  , RuleContent(..)
  , Rule(..), ruleTriggersIn, ruleContent
    , ruleRefs
  , RefData(..), rdScope, rdRenameHistory, rdRelations, rdBody, rdIsCircumsized, rdTriggers, rdRefs
    , defaultRefData
  , AppliedPiResult(..), aprPiGuid, aprArgVal, aprDestRef, aprCopiedNames, appliedPiResultRefs
  , ExprRefs(..), exprRefsUF, exprRefsData
  , RuleMap(..), rmNext, rmMap
    , newRule, ruleVerifyTagId
  , Context(..), ctxExprRefs, ctxDefTVs, ctxRuleMap, ctxRandomGen
    , emptyContext
  , LoadedDef(..), ldDef, ldType
  , TypedValue(..), tvVal, tvType, tvRefs
  , ScopedTypedValue(..), stvTV, stvScope, stvRefs
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref, RefMap, RefSet)
import qualified Control.Lens as Lens
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
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

-- We know of a GetField, waiting to know the record type:
data GetFieldPhase0 = GetFieldPhase0
  { _gf0GetFieldTag :: Ref
  , _gf0GetFieldType :: Ref
  -- trigger on record type, no need for Ref
  }
Lens.makeLenses ''GetFieldPhase0

gf0Refs :: Lens.Traversal' GetFieldPhase0 Ref
gf0Refs f (GetFieldPhase0 tag typ) =
  GetFieldPhase0 <$> f tag <*> f typ

-- We know of a GetField and the record type, waiting to know the
-- GetField tag:
data GetFieldPhase1 = GetFieldPhase1
  { _gf1GetFieldRecordTypeFields :: [(Ref, Ref)]
  , _gf1GetFieldType :: Ref
  -- trigger on getfield tag, no need for Ref
  }
Lens.makeLenses ''GetFieldPhase1

gf1Refs :: Lens.Traversal' GetFieldPhase1 Ref
gf1Refs f (GetFieldPhase1 rFields typ) =
  GetFieldPhase1 <$> (Lens.traverse . Lens.both) f rFields <*> f typ

-- We know of a GetField and the record type, waiting to know the
-- GetField tag (trigger on getfield tag):
data GetFieldPhase2 = GetFieldPhase2
  { _gf2Tag :: Guid
  , _gf2TagRef :: Ref
  , _gf2TypeRef :: Ref
  , -- Maps Refs of tags to Refs of their field types
    _gf2MaybeMatchers :: RefMap Ref
  }
Lens.makeLenses ''GetFieldPhase2

gf2Refs :: Lens.Traversal' GetFieldPhase2 Ref
gf2Refs f (GetFieldPhase2 tag tagRef typeRef mMatchers) =
  GetFieldPhase2 tag <$> f tagRef <*> f typeRef <*>
  (fmap IntMap.fromList . (Lens.traverse . Lens.both) f . IntMap.toList) mMatchers

data Rule = Rule
  { _ruleTriggersIn :: RefSet
  , _ruleContent :: RuleContent
  }

data RuleContent
  = RuleVerifyTag
  | RuleGetFieldPhase0 GetFieldPhase0
  | RuleGetFieldPhase1 GetFieldPhase1
  | RuleGetFieldPhase2 GetFieldPhase2
type RuleId = Int
type RuleIdMap = IntMap

Lens.makeLenses ''Rule

ruleContentRefs :: Lens.Traversal' RuleContent Ref
ruleContentRefs _ RuleVerifyTag = pure RuleVerifyTag
ruleContentRefs f (RuleGetFieldPhase0 x) = RuleGetFieldPhase0 <$> gf0Refs f x
ruleContentRefs f (RuleGetFieldPhase1 x) = RuleGetFieldPhase1 <$> gf1Refs f x
ruleContentRefs f (RuleGetFieldPhase2 x) = RuleGetFieldPhase2 <$> gf2Refs f x

ruleRefs :: Lens.Traversal' Rule Ref
ruleRefs f (Rule triggers content) =
  Rule
  <$> (fmap IntSet.fromList . Lens.traverse f . IntSet.toList) triggers
  <*> ruleContentRefs f content

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

data RuleMap = RuleMap
  { _rmNext :: Int
  , _rmMap :: RuleIdMap Rule
  }
Lens.makeLenses ''RuleMap

newRule :: Monad m => RuleContent -> StateT RuleMap m RuleId
newRule rule = do
  ruleId <- Lens.use rmNext
  rmMap . Lens.at ruleId .= Just (Rule mempty rule)
  rmNext += 1
  return ruleId

ruleVerifyTagId :: RuleId
ruleVerifyTagId = 0

initialRuleMap :: RuleMap
initialRuleMap = RuleMap
  { _rmNext = ruleVerifyTagId + 1
  , _rmMap = mempty & Lens.at ruleVerifyTagId .~ Just (Rule mempty RuleVerifyTag)
  }

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
