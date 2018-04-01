-- | Sugared evaluation results
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Eval
    ( EvalScopes
    , ChildScopes, ParamScopes, EvaluationScopes
    , ScopeId
    , EvalError(..)
    , ResRecord(..), recordFields
    , ResTable(..), rtHeaders, rtRows
    , ResTree(..), rtRoot, rtSubtrees
    , ResStream(..), rsHead
    , ResInject(..), riTag, riVal
    , ResBody(..)
    , _RRecord, _RInject, _RFunc, _RArray, _RError, _RBytes, _RFloat
    , _RStream, _RTree, _RText
    , ResVal(..), resPayload, resBody
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Lamdu.Data.Anchors (BinderParamScopeId)
import           Lamdu.Eval.Results (ScopeId, EvalError(..))
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

newtype ResRecord name v = ResRecord
    { _recordFields :: [(TagInfo name, v)]
    } deriving (Show, Functor, Foldable, Traversable)

data ResInject name v = ResInject
    { _riTag :: TagInfo name
    , _riVal :: Maybe v
    } deriving (Show, Functor, Foldable, Traversable)

data ResTree v = ResTree
    { _rtRoot :: v
    , _rtSubtrees :: [v]
    } deriving (Show, Functor, Foldable, Traversable)

data ResTable name v = ResTable
    { _rtHeaders :: [TagInfo name]
    , _rtRows :: [[v]] -- All rows are same length as each other and the headers
    } deriving (Show, Functor, Foldable, Traversable)

newtype ResStream v = ResStream
    { _rsHead :: v
    } deriving (Show, Functor, Foldable, Traversable)

data ResBody name v
    = RRecord (ResRecord name v)
    | RInject (ResInject name v)
    | RFunc Int -- Identifier for function instance
    | RArray [v] -- TODO: Vector here?
    | RError EvalError
    | RBytes ByteString
    | RFloat Double
    -- Sugared forms:
    | RTable (ResTable name v)
    | RStream (ResStream v)
    | RTree (ResTree v)
    | RText Text
    deriving (Show, Functor, Foldable, Traversable)

data ResVal name = ResVal
    { _resPayload :: EntityId
    , _resBody :: ResBody name (ResVal name)
    } deriving Show

type EvalScopes a = CurAndPrev (Map ScopeId a)

-- This is a mapping from a parent scope to the inner scope in:
-- * A redex lambda body (executed exactly once)
-- * Also used for if-else sugar where else-if scopes are executed no more than once
type ChildScopes = EvalScopes ScopeId

type ParamScopes = EvalScopes [BinderParamScopeId]

-- For parameters: if there were any applies-of-lam in a parent scope,
-- even if they got no values yet, it will be `Just mempty`, which
-- will not fall back to showing the prev
-- TODO: Does this actually happen? Do we generate empty lists of
-- scope-val pairs for lams?
type EvaluationScopes name = CurAndPrev (Maybe (Map ScopeId (ResVal name)))

Lens.makeLenses ''ResInject
Lens.makeLenses ''ResRecord
Lens.makeLenses ''ResStream
Lens.makeLenses ''ResTable
Lens.makeLenses ''ResTree
Lens.makeLenses ''ResVal
Lens.makePrisms ''ResBody
