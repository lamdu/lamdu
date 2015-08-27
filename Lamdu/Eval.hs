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
import           Control.Monad (void)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Control.MonadA (MonadA)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (EvalResult, Val(..), EvalError(..), Closure(..), Scope(..), emptyScope, ScopeId(..), scopeIdInt)
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

data ScopedVal pl = ScopedVal
    { _srcScope :: !(Scope pl)
    , _srcExpr :: !(V.Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

data EventLambdaApplied pl = EventLambdaApplied
    { elaLam :: pl
    , elaParentId :: !ScopeId
    , elaId :: !ScopeId
    , elaArgument :: !(EvalResult pl)
    } deriving (Show, Functor, Foldable, Traversable)

data EventResultComputed pl = EventResultComputed
    { ercSource :: pl
    , ercScope :: !ScopeId
    , ercResult :: !(EvalResult pl)
    } deriving (Show, Functor, Foldable, Traversable)

data Event pl
    = ELambdaApplied (EventLambdaApplied pl)
    | EResultComputed (EventResultComputed pl)
    deriving (Show, Functor, Foldable, Traversable)

data EvalActions m pl = EvalActions
    { _aReportEvent :: Event pl -> m ()
    , _aRunBuiltin :: Def.FFIName -> EvalResult pl -> EvalResult pl
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Def.Body (V.Val pl)))
    }

newtype Env m pl = Env
    { _eEvalActions :: EvalActions m pl
    }

data EvalState m pl = EvalState
    { _esScopeCounter :: !ScopeId
    , _esLoadedGlobals :: !(Map V.GlobalId (EvalResult pl))
    , _esReader :: !(Env m pl) -- This is ReaderT
    }

newtype EvalT pl m a = EvalT
    { runEvalT :: StateT (EvalState m pl) m a
    } deriving (Functor, Applicative, Monad)

liftState :: Monad m => StateT (EvalState m pl) m a -> EvalT pl m a
liftState = EvalT

instance MonadTrans (EvalT pl) where
    lift = liftState . lift

Lens.makeLenses ''Scope
Lens.makeLenses ''EvalActions
Lens.makeLenses ''Env
Lens.makeLenses ''EvalState

ask :: Monad m => EvalT pl m (Env m pl)
ask = use esReader & liftState

report :: MonadA m => Event pl -> EvalT pl m ()
report event =
    do
        rep <- ask <&> (^. eEvalActions . aReportEvent)
        rep event & lift

bindVar :: MonadA m => pl -> V.Var -> EvalResult pl -> Scope pl -> EvalT pl m (Scope pl)
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
    MonadA m => V.Apply (EvalResult pl) -> EvalT pl m (EvalResult pl)
evalApply (V.Apply (Left err) _) = Left err & return
evalApply (V.Apply (Right func) argEr) =
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
        Left err -> Left err & return
        Right (HInject (V.Inject sumTag injected))
            | caseTag == sumTag -> V.Apply handlerFunc injected & evalApply
            | otherwise         -> V.Apply rest        argEr    & evalApply
        Right x -> EvalTypeError ("Case expects Inject, found: " ++ show (void x)) & Left & return
    HAbsurd ->
        case argEr of
        Left err -> err
        Right x ->
            "Value impossibly typed as Void: " ++ show (void x) & EvalTypeError
        & Left & return
    _ ->
        "Apply expects func, builtin, or case, found: " ++ show (void func)
        & EvalTypeError & Left & return

evalGetField ::
    Monad m => V.GetField (EvalResult pl) -> EvalT pl m (EvalResult pl)
evalGetField (V.GetField (Left err) _) = Left err & return
evalGetField (V.GetField (Right (HRecExtend (V.RecExtend tag val rest))) searchTag)
    | searchTag == tag = return val
    | otherwise = V.GetField rest searchTag & evalGetField
evalGetField (V.GetField (Right x) y) =
    "GetField of " ++ show y ++ " expects record, found: " ++ show (void x)
    & EvalTypeError & Left & return

evalScopedVal :: MonadA m => ScopedVal pl -> EvalT pl m (EvalResult pl)
evalScopedVal (ScopedVal scope expr) =
    reportResultComputed =<<
    case expr ^. V.body of
    V.BAbs lam ->
        Closure scope lam (expr ^. V.payload) & HFunc & Right & return
    V.BApp apply -> traverse inner apply >>= evalApply
    V.BGetField getField -> traverse inner getField >>= evalGetField
    V.BInject    inject    -> traverse inner inject    <&> Right . HInject
    V.BRecExtend recExtend -> traverse inner recExtend <&> Right . HRecExtend
    V.BCase      case_     -> traverse inner case_     <&> Right . HCase
    V.BFromNom (V.Nom _ v) -> inner v
    V.BToNom   (V.Nom _ v) -> inner v
    V.BLeaf (V.LGlobal global) -> loadGlobal global
    V.BLeaf (V.LVar var) ->
        case scope ^. scopeMap . at var of
        Nothing ->
            "Variable " ++ show var ++ " out of scope"
            & EvalTypeError & Left & return
        Just val -> return val
    V.BLeaf V.LRecEmpty -> Right HRecEmpty & return
    V.BLeaf V.LAbsurd   -> Right HAbsurd & return
    V.BLeaf (V.LLiteral (V.Literal p bs))
        | p == Builtins.intId -> Builtins.decodeInt bs & HInteger & Right & return
        | otherwise -> error "TODO Literals which are not integers"
    V.BLeaf V.LHole -> Left EvalHole & return
    where
        inner = evalScopedVal . ScopedVal scope
        reportResultComputed result =
            do
                EventResultComputed (expr ^. V.payload) (scope ^. scopeId) result
                    & EResultComputed & report
                return result

loadGlobal :: MonadA m => V.GlobalId -> EvalT pl m (EvalResult pl)
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
                    Nothing -> EvalLoadGlobalFailed g & Left & return
                    Just (Def.BodyBuiltin (Def.Builtin name _t)) ->
                        HBuiltin name & Right & return
                    Just (Def.BodyExpr (Def.Expr expr _t)) ->
                        evalScopedVal $ ScopedVal emptyScope expr
                liftState $ esLoadedGlobals . at g ?= result
                return result

initialState :: Env m pl -> EvalState m pl
initialState env =
    EvalState
    { _esScopeCounter = ScopeId 1
    , _esLoadedGlobals = Map.empty
    , _esReader = env
    }
