{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Rule.Internal
  ( RuleId, RuleIdMap
  , GetFieldPhase0(..), gf0GetFieldTag, gf0GetFieldType
  , GetFieldPhase1(..), gf1GetFieldRecordTypeFields, gf1GetFieldType
  , GetFieldPhase2(..), gf2Tag, gf2TagRef, gf2TypeRef, gf2MaybeMatchers
  , Rule(..), ruleTriggersIn, ruleContent
    , ruleRefs
  , RuleContent(..)
  , RuleMap(..), rmNext, rmMap
    , new, verifyTagId
    , initialRuleMap
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Data.IntMap (IntMap)
import Data.Monoid (Monoid(..))
import Data.OpaqueRef (Ref, RefMap, RefSet)
import Data.Store.Guid (Guid)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR

type RuleId = Int
type RuleIdMap = IntMap

data RuleMap = RuleMap
  { _rmNext :: Int
  , _rmMap :: RuleIdMap Rule
  }

-- We know of a GetField, waiting to know the record type:
data GetFieldPhase0 = GetFieldPhase0
  { _gf0GetFieldTag :: Ref
  , _gf0GetFieldType :: Ref
  -- trigger on record type, no need for Ref
  }

-- We know of a GetField and the record type, waiting to know the
-- GetField tag:
data GetFieldPhase1 = GetFieldPhase1
  { _gf1GetFieldRecordTypeFields :: [(Ref, Ref)]
  , _gf1GetFieldType :: Ref
  -- trigger on getfield tag, no need for Ref
  }

-- We know of a GetField and the record type, waiting to know the
-- GetField tag (trigger on getfield tag):
data GetFieldPhase2 = GetFieldPhase2
  { _gf2Tag :: Guid
  , _gf2TagRef :: Ref
  , _gf2TypeRef :: Ref
  , -- Maps Refs of tags to Refs of their field types
    _gf2MaybeMatchers :: RefMap Ref
  }

data Rule = Rule
  { _ruleTriggersIn :: RefSet
  , _ruleContent :: RuleContent
  }

data RuleContent
  = RuleVerifyTag
  | RuleGetFieldPhase0 GetFieldPhase0
  | RuleGetFieldPhase1 GetFieldPhase1
  | RuleGetFieldPhase2 GetFieldPhase2

Lens.makeLenses ''RuleMap
Lens.makeLenses ''GetFieldPhase0
Lens.makeLenses ''GetFieldPhase1
Lens.makeLenses ''GetFieldPhase2
Lens.makeLenses ''Rule

gf0Refs :: Lens.Traversal' GetFieldPhase0 Ref
gf0Refs f (GetFieldPhase0 tag typ) =
  GetFieldPhase0 <$> f tag <*> f typ

gf1Refs :: Lens.Traversal' GetFieldPhase1 Ref
gf1Refs f (GetFieldPhase1 rFields typ) =
  GetFieldPhase1 <$> (Lens.traverse . Lens.both) f rFields <*> f typ

gf2Refs :: Lens.Traversal' GetFieldPhase2 Ref
gf2Refs f (GetFieldPhase2 tag tagRef typeRef mMatchers) =
  GetFieldPhase2 tag <$> f tagRef <*> f typeRef <*>
  (mMatchers & OR.unsafeRefMapItems . Lens.both %%~ f)

ruleContentRefs :: Lens.Traversal' RuleContent Ref
ruleContentRefs _ RuleVerifyTag = pure RuleVerifyTag
ruleContentRefs f (RuleGetFieldPhase0 x) = RuleGetFieldPhase0 <$> gf0Refs f x
ruleContentRefs f (RuleGetFieldPhase1 x) = RuleGetFieldPhase1 <$> gf1Refs f x
ruleContentRefs f (RuleGetFieldPhase2 x) = RuleGetFieldPhase2 <$> gf2Refs f x

ruleRefs :: Lens.Traversal' Rule Ref
ruleRefs f (Rule triggers content) =
  Rule
  <$> OR.unsafeRefSetKeys f triggers
  <*> ruleContentRefs f content

new :: Monad m => RuleContent -> StateT RuleMap m RuleId
new rule = do
  ruleId <- Lens.use rmNext
  rmMap . Lens.at ruleId .= Just (Rule mempty rule)
  rmNext += 1
  return ruleId

verifyTagId :: RuleId
verifyTagId = 0

initialRuleMap :: RuleMap
initialRuleMap = RuleMap
  { _rmNext = verifyTagId + 1
  , _rmMap = mempty & Lens.at verifyTagId .~ Just (Rule mempty RuleVerifyTag)
  }
