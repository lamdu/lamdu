-- | Sugared evaluation results
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Eval
    ( EvalScopes
    , ParamScopes, EvaluationScopes
    , ScopeId
    , ER.EvalTypeError(..)
    , ER.CompiledErrorType(..)
        , ER._DependencyTypeOutOfDate, ER._ReachedHole, ER._UnhandledCase
    , ER.Error(..), ER._CompiledError, ER._RuntimeError
    , EvalException(..), evalExceptionType, evalExceptionJumpTo
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
    { _recordFields :: [(Tag name, v)]
    } deriving stock (Functor, Foldable, Traversable, Generic)

data ResInject name v = ResInject
    { _riTag :: Tag name
    , _riVal :: Maybe v
    } deriving (Functor, Foldable, Traversable, Generic)

data ResTree v = ResTree
    { _rtRoot :: v
    , _rtSubtrees :: [v]
    } deriving (Functor, Foldable, Traversable, Generic)

data ResTable name v = ResTable
    { _rtHeaders :: [Tag name]
    , _rtRows :: [[v]] -- All rows are same length as each other and the headers
    } deriving (Functor, Foldable, Traversable, Generic)

newtype ResList v = ResList
    { _rsHead :: v
    } deriving stock (Functor, Foldable, Traversable, Generic)

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
    deriving (Functor, Foldable, Traversable, Generic)

data ResVal name = ResVal
    { _resPayload :: EntityId
    , _resBody :: ResBody name (ResVal name)
    } deriving (Generic)

type EvalScopes a = CurAndPrev (Map ScopeId a)

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
    { _evalExceptionType :: ER.Error
    , _evalExceptionJumpTo :: Maybe (o EntityId)
    } deriving Generic

data EvalCompletionResult o
    = EvalSuccess
    | EvalError (EvalException o)
    deriving Generic

type EvalCompletion o = CurAndPrev (Maybe (EvalCompletionResult o))

traverse Lens.makeLenses
    [''EvalException, ''ResInject, ''ResRecord, ''ResList, ''ResTable, ''ResTree, ''ResVal]
    <&> concat
Lens.makePrisms ''EvalCompletionResult
Lens.makePrisms ''ResBody
