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
    , ResVal(..), resBody
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (BinderParamScopeId)
import           Lamdu.Eval.Results (ScopeId, EvalError(..))
-- import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

newtype ResRecord v = ResRecord
    { _recordFields :: [(T.Tag, v)]
    } deriving (Show, Functor, Foldable, Traversable)

data ResInject v = ResInject
    { _riTag :: T.Tag -- TODO: TagInfo name
    , _riVal :: Maybe v
    } deriving (Show, Functor, Foldable, Traversable)

data ResTree v = ResTree
    { _rtRoot :: v
    , _rtSubtrees :: [v]
    } deriving (Show, Functor, Foldable, Traversable)

data ResTable v = ResTable
    { _rtHeaders :: [T.Tag]
    , _rtRows :: [[v]] -- All rows are same length as each other and the headers
    } deriving (Show, Functor, Foldable, Traversable)

newtype ResStream v = ResStream
    { _rsHead :: v
    } deriving (Show, Functor, Foldable, Traversable)

data ResBody v
    = RRecord (ResRecord v)
    | RInject (ResInject v)
    | RFunc Int -- Identifier for function instance
    | RArray [v] -- TODO: Vector here?
    | RError EvalError
    | RBytes ByteString
    | RFloat Double
    -- Sugared forms:
    | RTable (ResTable v)
    | RStream (ResStream v)
    | RTree (ResTree v)
    | RText Text
    deriving (Show, Functor, Foldable, Traversable)

newtype ResVal = ResVal { _resBody :: ResBody ResVal }
    deriving Show

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
type EvaluationScopes = CurAndPrev (Maybe (Map ScopeId ResVal))

Lens.makeLenses ''ResInject
Lens.makeLenses ''ResRecord
Lens.makeLenses ''ResStream
Lens.makeLenses ''ResTable
Lens.makeLenses ''ResTree
Lens.makeLenses ''ResVal
Lens.makePrisms ''ResBody
