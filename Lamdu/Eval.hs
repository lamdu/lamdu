{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Eval
    ( EvalT(..), evalError
    , EvalState, initialState
    , ask
    , EvalActions(..)
    , Env(..), eEvalActions
    , Event(..), EventLambdaApplied(..), EventResultComputed(..)
    , ScopedVal(..)
    , evalScopedVal
    ) where

import           Prelude.Compat

import           Control.Lens (at, use)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Either (EitherT(..), left)
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (Val(..), Closure(..), Scope(..), emptyScope, ScopeId(..), scopeIdInt)
import qualified Lamdu.Expr.Val as V

data ScopedVal pl = ScopedVal
    { _srcScope :: !(Scope pl)
    , _srcExpr :: !(V.Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

data EventLambdaApplied pl = EventLambdaApplied
    { elaLam :: pl
    , elaParentId :: !ScopeId
    , elaId :: !ScopeId
    , elaArgument :: !(Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

data EventResultComputed pl = EventResultComputed
    { ercSource :: pl
    , ercScope :: !ScopeId
    , ercResult :: !(Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

data Event pl
    = ELambdaApplied (EventLambdaApplied pl)
    | EResultComputed (EventResultComputed pl)
    deriving (Show, Functor, Foldable, Traversable)

data EvalActions m pl = EvalActions
    { _aReportEvent :: Event pl -> m ()
    , _aRunBuiltin :: Def.FFIName -> Val pl -> Val pl
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Def.Body (V.Val pl)))
    }

newtype Env m pl = Env
    { _eEvalActions :: EvalActions m pl
    }

data EvalState m pl = EvalState
    { _esScopeCounter :: !ScopeId
    , _esLoadedGlobals :: !(Map V.GlobalId (Val pl))
    , _esReader :: !(Env m pl) -- This is ReaderT
    }

newtype EvalT pl m a = EvalT
    { runEvalT :: EitherT String (StateT (EvalState m pl) m) a
    } deriving (Functor, Applicative, Monad)

liftState :: Monad m => StateT (EvalState m pl) m a -> EvalT pl m a
liftState = EvalT . lift

instance MonadTrans (EvalT pl) where
    lift = liftState . lift

Lens.makeLenses ''Scope
Lens.makeLenses ''EvalActions
Lens.makeLenses ''Env
Lens.makeLenses ''EvalState

ask :: Monad m => EvalT pl m (Env m pl)
ask = use esReader & liftState

report :: Monad m => Event pl -> EvalT pl m ()
report event =
    do
        rep <- ask <&> (^. eEvalActions . aReportEvent)
        rep event & lift

bindVar :: Monad m => pl -> V.Var -> Val pl -> Scope pl -> EvalT pl m (Scope pl)
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
            , _scopeMap = parentMap & at var .~ Just val
            } & return

evalError :: Monad m => String -> EvalT pl m a
evalError = EvalT . left

evalApply :: Monad m => V.Apply (Val pl) -> EvalT pl m (Val pl)
evalApply (V.Apply func arg) =
    case func of
    HFunc (Closure outerScope (V.Lam var body) lamPl) ->
        bindVar lamPl var arg outerScope
        >>= evalScopedVal . (`ScopedVal` body)
    HBuiltin ffiname
        | containsError arg -> return HError
        | otherwise ->
            do
                runBuiltin <- ask <&> (^. eEvalActions . aRunBuiltin)
                runBuiltin ffiname arg & return
        where
            -- only supports records because that's what builtins handle..
            containsError HError = True
            containsError (HRecExtend (V.RecExtend _ v r)) =
                containsError v || containsError r
            containsError _ = False
    HCase (V.Case caseTag handlerFunc rest) ->
        case arg of
        HInject (V.Inject sumTag injected)
            | caseTag == sumTag -> V.Apply handlerFunc injected & evalApply
            | otherwise -> V.Apply rest arg & evalApply
        _ -> evalError "Case applied on non sum-type"
    HError -> return HError
    _ -> evalError "Apply on non function: "

evalScopedVal :: Monad m => ScopedVal pl -> EvalT pl m (Val pl)
evalScopedVal (ScopedVal scope expr) =
    reportResultComputed =<<
    case expr ^. V.body of
    V.BAbs lam -> return $ HFunc $ Closure scope lam (expr ^. V.payload)
    V.BApp apply -> traverse inner apply >>= evalApply
    V.BGetField getField -> traverse inner getField >>= evalGetField
    V.BInject    inject    -> traverse inner inject    <&> HInject
    V.BRecExtend recExtend -> traverse inner recExtend <&> HRecExtend
    V.BCase      case_     -> traverse inner case_     <&> HCase
    V.BLeaf (V.LGlobal global) -> loadGlobal global
    V.BLeaf (V.LVar var) ->
        case scope ^. scopeMap . at var of
        Nothing -> evalError $ "Variable out of scope: " ++ show var
        Just val -> return val
    V.BLeaf V.LRecEmpty -> return HRecEmpty
    V.BLeaf V.LAbsurd   -> return HAbsurd
    V.BLeaf (V.LLiteralInteger i) -> HInteger i & return
    V.BLeaf V.LHole -> return HError
    V.BFromNom (V.Nom _ v) -> inner v
    V.BToNom   (V.Nom _ v) -> inner v
    where
        inner = evalScopedVal . ScopedVal scope
        reportResultComputed result =
            do
                EventResultComputed (expr ^. V.payload) (scope ^. scopeId) result
                    & EResultComputed & report
                return result

evalGetField :: Monad m => V.GetField (Val pl) -> EvalT pl m (Val pl)
evalGetField (V.GetField (HRecExtend (V.RecExtend tag val rest)) searchTag)
    | searchTag == tag = return val
    | otherwise = V.GetField rest searchTag & evalGetField
evalGetField (V.GetField val _) =
    evalError $ "GetField of value without the field " ++ show (void val)

loadGlobal :: Monad m => V.GlobalId -> EvalT pl m (Val pl)
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
                    Nothing -> evalError $ "Global not found " ++ show g
                    Just (Def.BodyBuiltin (Def.Builtin name _t)) ->
                        return $ HBuiltin name
                    Just (Def.BodyExpr (Def.Expr expr _t)) ->
                        evalScopedVal $ ScopedVal emptyScope expr
                liftState $ esLoadedGlobals . at g .= Just result
                return result

initialState :: Env m pl -> EvalState m pl
initialState env =
    EvalState
    { _esScopeCounter = ScopeId 1
    , _esLoadedGlobals = Map.empty
    , _esReader = env
    }
