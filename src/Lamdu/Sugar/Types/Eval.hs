-- | Sugared evaluation results
{-# LANGUAGE TemplateHaskell, GADTs, TypeFamilies, MultiParamTypeClasses #-}
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
    , ResTable(..), rtHeaders, rtRows
    , ResTree(..), rtRoot, rtSubtrees
    , ResInject(..), riTag, riVal
    , Result(..)
    , _RRecord, _RInject, _RFunc, _RArray, _RError, _RBytes, _RFloat
    , _RList, _RTree, _RText
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import           Hyper
import           Lamdu.Data.Anchors (BinderParamScopeId)
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

data ResInject name h = ResInject
    { _riTag :: Tag name
    , _riVal :: Maybe (h :# Result name)
    } deriving Generic

data ResTree name h = ResTree
    { _rtRoot :: h :# Result name
    , _rtSubtrees :: [h :# Result name]
    } deriving Generic

data ResTable name h = ResTable
    { _rtHeaders :: [Tag name]
    , _rtRows :: [[h :# Result name]] -- All rows are same length as each other and the headers
    } deriving Generic

data Result name h
    = RRecord [(Tag name, h :# Result name)]
    | RInject (ResInject name h)
    | RFunc Int -- Identifier for function instance
    | RArray [h :# Result name]
    | RError ER.EvalTypeError
    | RBytes ByteString
    | RFloat Double
    -- Sugared forms:
    | RTable (ResTable name h)
    | RList (h :# Result name) -- Only head is visible
    | RTree (ResTree name h)
    | RText Text
    | RChar Char
    deriving Generic

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
type EvaluationScopes name i = CurAndPrev (Maybe (Map ScopeId (i (Annotated EntityId # Result name))))

data EvalException o = EvalException
    { _evalExceptionType :: ER.Error
    , _evalExceptionJumpTo :: Maybe (o EntityId)
    } deriving Generic

data EvalCompletionResult o
    = EvalSuccess
    | EvalError (EvalException o)
    deriving Generic

type EvalCompletion o = CurAndPrev (Maybe (EvalCompletionResult o))

traverse Lens.makeLenses [''EvalException, ''ResInject, ''ResTable, ''ResTree] <&> concat
Lens.makePrisms ''EvalCompletionResult
Lens.makePrisms ''Result

traverse makeHTraversableAndBases [''Result, ''ResTree, ''ResInject, ''ResTable] <&> concat
traverse makeHMorph [''ResTree] <&> concat