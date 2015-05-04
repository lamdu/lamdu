{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Eval
    ( EvalT(..), evalError
    , EvalState, initialState
    , EvalActions(..), Event(..), EventNewScope(..), EventResultComputed(..)
    , ScopedVal(..)
    , whnfScopedVal, whnfThunk
    ) where

import Control.Applicative (Applicative)
import Control.Lens (at, use, traverse)
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..), left)
import Control.Monad.Trans.State (StateT(..))
import Data.Map (Map)
import Lamdu.Data.Definition (FFIName)
import Lamdu.Eval.Val (ValHead(..), ThunkId, Closure(..), Scope(..), ScopeId, emptyScope)
import Lamdu.Expr.Val (Val)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Expr.Val as V

data ThunkState pl
    = TSrc (ScopedVal pl)
    | TResult (ValHead pl)
    | TEvaluating -- BlackHoled in GHC
    deriving (Functor, Show)

data ScopedVal pl = ScopedVal
    { _srcScope :: Scope
    , _srcExpr :: Val pl
    } deriving (Functor, Show)

data EventNewScope pl = EventNewScope
    { ensLam :: pl
    , ensParentId :: ScopeId
    , ensId :: ScopeId
    } deriving (Show)

data EventResultComputed pl = EventResultComputed
    { ercMThunkId :: Maybe ThunkId
    , ercSource :: pl
    , ercScope :: ScopeId
    , ercResult :: ValHead pl
    } deriving (Show)

data Event pl
    = ENewScope (EventNewScope pl)
    | EResultComputed (EventResultComputed pl)
    deriving (Show)

data EvalActions m pl = EvalActions
    { _aReportEvent :: Event pl -> m ()
    , _aRunBuiltin :: FFIName -> ThunkId -> EvalT pl m (ValHead pl)
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Either FFIName (Val pl)))
    }

data EvalState m pl = EvalState
    { _esThunks :: Map ThunkId (ThunkState pl)
    , _esThunkCounter :: ThunkId
    , _esScopeCounter :: ScopeId
    , _esLoadedGlobals :: Map V.GlobalId (ValHead pl)
    , _esReader :: EvalActions m pl -- This is ReaderT
    }

newtype EvalT pl m a = EvalT
    { runEvalT :: EitherT String (StateT (EvalState m pl) m) a
    } deriving (Functor, Applicative, Monad, MonadState (EvalState m pl))

instance MonadTrans (EvalT pl) where
    lift = EvalT . lift . lift

Lens.makeLenses ''Scope
Lens.makeLenses ''EvalActions
Lens.makeLenses ''EvalState

freshThunkId :: Monad m => EvalT pl m ThunkId
freshThunkId =
    do
        thunkId <- use esThunkCounter
        esThunkCounter += 1
        return thunkId

report :: Monad m => Event pl -> EvalT pl m ()
report event =
    do
        rep <- use $ esReader . aReportEvent
        rep event & lift

bindVar :: Monad m => pl -> V.Var -> ThunkId -> Scope -> EvalT pl m Scope
bindVar lamPl var val (Scope parentMap parentId) =
    do
        newScopeId <- use esScopeCounter
        esScopeCounter += 1
        EventNewScope
            { ensLam = lamPl
            , ensParentId = parentId
            , ensId = newScopeId
            } & ENewScope & report
        Scope
            { _scopeId = newScopeId
            , _scopeMap = parentMap & at var .~ Just val
            } & return

evalError :: Monad m => String -> EvalT pl m a
evalError = EvalT . left

whnfScopedValInner :: Monad m => Maybe ThunkId -> ScopedVal pl -> EvalT pl m (ValHead pl)
whnfScopedValInner mThunkId (ScopedVal scope expr) =
    reportResultComputed =<<
    case expr ^. V.body of
    V.BAbs lam -> return $ HFunc $ Closure scope lam (expr ^. V.payload)
    V.BApp (V.Apply funcExpr argExpr) ->
        do
            func <- whnfScopedVal $ ScopedVal scope funcExpr
            argThunk <- makeThunk (ScopedVal scope argExpr)
            case func of
                HFunc (Closure outerScope (V.Lam var body) lamPl) ->
                    do
                        innerScope <- bindVar lamPl var argThunk outerScope
                        whnfScopedVal (ScopedVal innerScope body)
                HBuiltin ffiname ->
                    do
                        runBuiltin <- use $ esReader . aRunBuiltin
                        runBuiltin ffiname argThunk
                _ -> evalError "Apply on non function"
    V.BGetField (V.GetField recordExpr tag) ->
        do
            record <- whnfScopedVal $ ScopedVal scope recordExpr
            whnfGetField $ V.GetField record tag
    V.BRecExtend recExtend ->
        recExtend & traverse %%~ makeThunk . ScopedVal scope <&> HRecExtend
    V.BLeaf (V.LGlobal global) -> loadGlobal global
    V.BLeaf (V.LVar var) ->
        case scope ^. scopeMap . at var of
        Nothing -> evalError $ "Variable out of scope: " ++ show var
        Just thunkId -> whnfThunk thunkId
    V.BLeaf V.LRecEmpty -> return HRecEmpty
    V.BLeaf (V.LLiteralInteger i) -> HInteger i & return
    V.BLeaf V.LHole -> evalError "Hole"
    where
        reportResultComputed result =
            do
                EventResultComputed mThunkId (expr ^. V.payload) (scope ^. scopeId) result
                    & EResultComputed & report
                return result

whnfScopedVal :: Monad m => ScopedVal pl -> EvalT pl m (ValHead pl)
whnfScopedVal = whnfScopedValInner Nothing

makeThunk :: Monad m => ScopedVal pl -> EvalT pl m ThunkId
makeThunk src =
    do
        thunkId <- freshThunkId
        esThunks . at thunkId .= Just (TSrc src)
        return thunkId

whnfGetField :: Monad m => V.GetField (ValHead pl) -> EvalT pl m (ValHead pl)
whnfGetField (V.GetField (HRecExtend (V.RecExtend otherTag otherVal restRef)) tag)
    | tag == otherTag = whnfThunk otherVal
    | otherwise =
        whnfThunk restRef
        <&> (`V.GetField` tag)
        >>= whnfGetField
whnfGetField (V.GetField val _) =
    evalError $ "GetField of value without the field " ++ show (void val)

whnfThunk :: Monad m => ThunkId -> EvalT pl m (ValHead pl)
whnfThunk thunkId = do
    thunkState <- use $ esThunks . at thunkId
    case thunkState of
        Just TEvaluating -> evalError "*INFINITE LOOP*"
        Just (TResult r) -> return r
        Just (TSrc thunkSrc) ->
            do
                res <- whnfScopedValInner (Just thunkId) thunkSrc
                esThunks . at thunkId .= Just (TResult res)
                return res
        Nothing -> evalError $ "BUG: Referenced non-existing thunk " ++ show thunkId

loadGlobal :: Monad m => V.GlobalId -> EvalT pl m (ValHead pl)
loadGlobal g =
    do
        loaded <- use (esLoadedGlobals . at g)
        case loaded of
            Just cached -> return cached
            Nothing -> do
                loader <- use $ esReader . aLoadGlobal
                mLoadedGlobal <- lift $ loader g
                result <-
                    case mLoadedGlobal of
                    Nothing -> evalError $ "Global not found " ++ show g
                    Just (Left name) -> return $ HBuiltin name
                    Just (Right expr) -> whnfScopedVal $ ScopedVal emptyScope expr
                esLoadedGlobals . at g .= Just result
                return result

initialState :: EvalActions m pl -> EvalState m pl
initialState actions =
    EvalState
    { _esThunks = Map.empty
    , _esThunkCounter = 0
    , _esScopeCounter = 1
    , _esLoadedGlobals = Map.empty
    , _esReader = actions
    }
