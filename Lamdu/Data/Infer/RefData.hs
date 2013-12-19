{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Infer.RefData
  ( Restriction(..)
  , LoadedDef(..), ldDef, ldType
  , LoadedBody, LoadedExpr
  , RefData(..), rdScope, rdBody, rdWasNotDirectlyTag, rdRestrictions, rdTriggers
    , defaultRefData
  , Scope(..), scopeMap, scopeMDef
    , emptyScope
    , scopeParamRefs
    , scopeNormalizeParamRefs
  , UFExprs
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Monoid (Monoid(..))
import Data.Monoid.Instances () -- Binary Any
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.UnionFind.WithData (UFData)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefTags (TagExpr, ExprRef, ParamRef, TagRule, TagParam)
import Lamdu.Data.Infer.Trigger.Types (Trigger)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.OpaqueRef as OR
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases

data Scope def = Scope
  { _scopeMap :: OR.RefMap (TagParam def) (ExprRef def)
  , -- is Just if it belongs to one exclusive Def (and can later see
    -- its implicit variables):
    _scopeMDef :: Maybe def
  }
Lens.makeLenses ''Scope
derive makeBinary ''Scope

emptyScope :: def -> Scope def
emptyScope def = Scope
  { _scopeMap = OR.refMapEmpty
  , _scopeMDef = Just def
  }

data Restriction def
  = MustMatch (ExprRef def)
  | MustBeRecordType
  | MustBeTag
  deriving (Eq, Show)
derive makeBinary ''Restriction

data LoadedDef def = LoadedDef
  { _ldDef :: def
  , _ldType :: ExprRef def
  } deriving (Show, Typeable)
Lens.makeLenses ''LoadedDef
derive makeBinary ''LoadedDef

instance Eq def => Eq (LoadedDef def) where
  LoadedDef a _ == LoadedDef b _ = a == b

type LoadedBody def = Expr.Body (LoadedDef def)
type LoadedExpr def = Expr.Expr (LoadedDef def)

data RefData def = RefData
  { _rdScope :: Scope def
  , _rdWasNotDirectlyTag :: Monoid.Any
  , _rdTriggers :: OR.RefMap (TagRule def) (Set (Trigger def))
  , _rdRestrictions :: [Restriction def]
  , _rdBody :: Expr.Body (LoadedDef def) (ExprRef def)
  }
Lens.makeLenses ''RefData
derive makeBinary ''RefData

type UFExprs def = UFData (TagExpr def) (RefData def)

defaultRefData :: Scope def -> Expr.Body (LoadedDef def) (ExprRef def) -> RefData def
defaultRefData scop body = RefData
  { _rdScope = scop
  , _rdWasNotDirectlyTag = Monoid.Any False
  , _rdTriggers = mempty
  , _rdRestrictions = []
  , _rdBody = body
  }

scopeParamRefs :: Lens.Traversal' (Scope def) (ParamRef def)
scopeParamRefs = scopeMap . OR.unsafeRefMapItems . Lens._1

scopeNormalizeParamRefs :: MonadA m => Scope def -> StateT (GuidAliases def) m (Scope def)
scopeNormalizeParamRefs = scopeParamRefs %%~ GuidAliases.find
