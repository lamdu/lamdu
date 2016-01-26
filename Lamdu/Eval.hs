{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Eval
    ( EvalT(..)
    , EvalState, initialState
    , ask
    , EvalActions(..)
    , Env(..), eEvalActions
    , Event(..), EventLambdaApplied(..), EventResultComputed(..)
    , ScopedVal(..)
    , evalScopedVal
    ) where

import           Control.Lens (at, use)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Control.MonadA (MonadA)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (Val(..), EvalError(..), Closure(..), Scope(..), emptyScope, ScopeId(..), scopeIdInt)
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

data ScopedVal srcId = ScopedVal
    { _srcScope :: !(Scope srcId)
    , _srcExpr :: !(V.Val srcId)
    } deriving (Show, Functor, Foldable, Traversable)

data EventLambdaApplied srcId = EventLambdaApplied
    { elaLam :: srcId
    , elaParentId :: !ScopeId
    , elaId :: !ScopeId
    , elaArgument :: !(Val srcId)
    } deriving (Show, Functor, Foldable, Traversable)

data EventResultComputed srcId = EventResultComputed
    { ercSource :: srcId
    , ercScope :: !ScopeId
    , ercResult :: !(Val srcId)
    } deriving (Show, Functor, Foldable, Traversable)

data Event srcId
    = ELambdaApplied (EventLambdaApplied srcId)
    | EResultComputed (EventResultComputed srcId)
    deriving (Show, Functor, Foldable, Traversable)

data EvalActions m srcId = EvalActions
    { _aReportEvent :: Event srcId -> m ()
    , _aRunBuiltin :: Def.FFIName -> Val srcId -> Val srcId
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Def.Body (V.Val srcId)))
    }

newtype Env m srcId = Env
    { _eEvalActions :: EvalActions m srcId
    }

data EvalState m srcId = EvalState
    { _esScopeCounter :: !ScopeId
    , _esLoadedGlobals :: !(Map V.GlobalId (Val srcId))
    , _esReader :: !(Env m srcId) -- This is ReaderT
    }

newtype EvalT srcId m a = EvalT
    { runEvalT :: StateT (EvalState m srcId) m a
    } deriving (Functor, Applicative, Monad)

liftState :: Monad m => StateT (EvalState m srcId) m a -> EvalT srcId m a
liftState = EvalT

instance MonadTrans (EvalT srcId) where
    lift = liftState . lift

Lens.makeLenses ''Scope
Lens.makeLenses ''EvalActions
Lens.makeLenses ''Env
Lens.makeLenses ''EvalState

ask :: Monad m => EvalT srcId m (Env m srcId)
ask = use esReader & liftState

report :: MonadA m => Event srcId -> EvalT srcId m ()
report event =
    do
        rep <- ask <&> (^. eEvalActions . aReportEvent)
        rep event & lift

bindVar :: MonadA m => srcId -> V.Var -> Val srcId -> Scope srcId -> EvalT srcId m (Scope srcId)
bindVar lamPl var val (Scope parentMap parentId) =
    do
        newScopeId <- liftState $ use esScopeCounter
        liftState $ esScopeCounter . scopeIdInt += 1
        EventLambdaApplied
            { elaLam = lamPl
            , elaParentId = parentId
            , elaId = newScopeId
            , elaArgument = val
            } & ELambdaApplied & report
        Scope
            { _scopeId = newScopeId
            , _scopeMap = parentMap & at var ?~ val
            } & return

evalApply ::
    MonadA m => V.Apply (Val srcId) -> EvalT srcId m (Val srcId)
evalApply (V.Apply func argEr) =
    case func of
    HFunc (Closure outerScope (V.Lam var body) lamPl) ->
        bindVar lamPl var argEr outerScope
        >>= evalScopedVal . (`ScopedVal` body)
    HBuiltin ffiname ->
        do
            runBuiltin <- ask <&> (^. eEvalActions . aRunBuiltin)
            runBuiltin ffiname argEr & return
    HCase (V.Case caseTag handlerFunc rest) ->
        case argEr of
        HInject (V.Inject sumTag injected)
            | caseTag == sumTag -> V.Apply handlerFunc injected & evalApply
            | otherwise         -> V.Apply rest        argEr    & evalApply
        HError err -> HError err & return
        x -> EvalTypeError ("Case expects Inject, found: " ++ show x) & HError & return
    HAbsurd ->
        case argEr of
        HError err -> err
        x ->
            "Value impossibly typed as Void: " ++ show x & EvalTypeError
        & HError & return
    HError err -> HError err & return
    _ ->
        "Apply expects func, builtin, or case, found: " ++ show func
        & EvalTypeError & HError & return

evalGetField ::
    Monad m => V.GetField (Val srcId) -> EvalT srcId m (Val srcId)
evalGetField (V.GetField (HError err) _) = HError err & return
evalGetField (V.GetField (HRecExtend (V.RecExtend tag val rest)) searchTag)
    | searchTag == tag = return val
    | otherwise = V.GetField rest searchTag & evalGetField
evalGetField (V.GetField x y) =
    "GetField of " ++ show y ++ " expects record, found: " ++ show x
    & EvalTypeError & HError & return

evalScopedVal :: MonadA m => ScopedVal srcId -> EvalT srcId m (Val srcId)
evalScopedVal (ScopedVal scope expr) =
    reportResultComputed =<<
    case expr ^. V.body of
    V.BAbs lam ->
        Closure scope lam (expr ^. V.payload) & HFunc & return
    V.BApp apply -> traverse inner apply >>= evalApply
    V.BGetField getField -> traverse inner getField >>= evalGetField
    V.BInject    inject    -> traverse inner inject    <&> HInject
    V.BRecExtend recExtend -> traverse inner recExtend <&> HRecExtend
    V.BCase      case_     -> traverse inner case_     <&> HCase
    V.BFromNom (V.Nom _ v) -> inner v
    V.BToNom   (V.Nom _ v) -> inner v
    V.BLeaf (V.LGlobal global) -> loadGlobal global
    V.BLeaf (V.LVar var) ->
        case scope ^. scopeMap . at var of
        Nothing ->
            "Variable " ++ show var ++ " out of scope"
            & EvalTypeError & HError & return
        Just val -> return val
    V.BLeaf V.LRecEmpty -> HRecEmpty & return
    V.BLeaf V.LAbsurd   -> HAbsurd & return
    V.BLeaf (V.LLiteral literal) -> HLiteral literal & return
    V.BLeaf V.LHole -> HError EvalHole & return
    where
        inner = evalScopedVal . ScopedVal scope
        reportResultComputed result =
            do
                EventResultComputed (expr ^. V.payload) (scope ^. scopeId) result
                    & EResultComputed & report
                return result

loadGlobal :: MonadA m => V.GlobalId -> EvalT srcId m (Val srcId)
loadGlobal g =
    do
        loaded <- liftState $ use (esLoadedGlobals . at g)
        case loaded of
            Just cached -> return cached
            Nothing -> do
                loader <- ask <&> (^. eEvalActions . aLoadGlobal)
                mLoadedGlobal <- lift $ loader g
                result <-
                    case mLoadedGlobal of
                    Nothing -> EvalLoadGlobalFailed g & HError & return
                    Just (Def.BodyBuiltin (Def.Builtin name _t)) ->
                        HBuiltin name & return
                    Just (Def.BodyExpr defExpr) ->
                        defExpr ^. Def.expr
                        & ScopedVal emptyScope & evalScopedVal
                liftState $ esLoadedGlobals . at g ?= result
                return result

initialState :: Env m srcId -> EvalState m srcId
initialState env =
    EvalState
    { _esScopeCounter = ScopeId 1
    , _esLoadedGlobals = Map.empty
    , _esReader = env
    }
