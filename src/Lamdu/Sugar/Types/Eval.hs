-- | Sugared evaluation results
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Eval
    ( EvalScopes
    , ChildScopes, ParamScopes, EvaluationScopes
    , ScopeId
    , ER.EvalTypeError(..)
    , ER.ErrorType(..)
    , EvalException(..)
        , evalExceptionType, evalExceptionDesc, evalExceptionJumpTo
    , EvalCompletionResult(..), _EvalSuccess, _EvalError
    , EvalCompletion
    , ResRecord(..), recordFields
    , ResTable(..), rtHeaders, rtRows
    , ResTree(..), rtRoot, rtSubtrees
    , ResList(..), rsHead
    , ResInject(..), riTag, riVal
    , ResBody(..)
    , _RRecord, _RInject, _RFunc, _RArray, _RError, _RBytes, _RFloat
    , _RList, _RTree, _RText
    , ResVal(..), resPayload, resBody
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Lamdu.Data.Anchors (BinderParamScopeId)
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

newtype ResRecord name v = ResRecord
    { _recordFields :: [(TagInfo name, v)]
    } deriving (Show, Functor, Foldable, Traversable, Generic)

data ResInject name v = ResInject
    { _riTag :: TagInfo name
    , _riVal :: Maybe v
    } deriving (Show, Functor, Foldable, Traversable, Generic)

data ResTree v = ResTree
    { _rtRoot :: v
    , _rtSubtrees :: [v]
    } deriving (Show, Functor, Foldable, Traversable, Generic)

data ResTable name v = ResTable
    { _rtHeaders :: [TagInfo name]
    , _rtRows :: [[v]] -- All rows are same length as each other and the headers
    } deriving (Show, Functor, Foldable, Traversable, Generic)

newtype ResList v = ResList
    { _rsHead :: v
    } deriving (Show, Functor, Foldable, Traversable, Generic)

data ResBody name v
    = RRecord (ResRecord name v)
    | RInject (ResInject name v)
    | RFunc Int -- Identifier for function instance
    | RArray [v] -- TODO: Vector here?
    | RError ER.EvalTypeError
    | RBytes ByteString
    | RFloat Double
    -- Sugared forms:
    | RTable (ResTable name v)
    | RList (ResList v)
    | RTree (ResTree v)
    | RText Text
    deriving (Show, Functor, Foldable, Traversable, Generic)

data ResVal name = ResVal
    { _resPayload :: EntityId
    , _resBody :: ResBody name (ResVal name)
    } deriving (Show, Generic)

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
--
-- Values are wrapped in an "i" action to avoid unnecessarily passing on all
-- values during the names pass.
type EvaluationScopes name i = CurAndPrev (Maybe (Map ScopeId (i (ResVal name))))

data EvalException o = EvalException
    { _evalExceptionType :: ER.ErrorType
    , _evalExceptionDesc :: Text
    , _evalExceptionJumpTo :: Maybe (o EntityId)
    } deriving Generic

data EvalCompletionResult name o
    = EvalSuccess (ResVal name)
    | EvalError (EvalException o)
    deriving Generic

type EvalCompletion name o = CurAndPrev (Maybe (EvalCompletionResult name o))

Lens.makeLenses ''EvalException
Lens.makeLenses ''ResInject
Lens.makeLenses ''ResRecord
Lens.makeLenses ''ResList
Lens.makeLenses ''ResTable
Lens.makeLenses ''ResTree
Lens.makeLenses ''ResVal
Lens.makePrisms ''EvalCompletionResult
Lens.makePrisms ''ResBody
