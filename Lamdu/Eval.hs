{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Eval
    ( EvalT(..), evalError
    , EvalState, initialState
    , EvalActions(..), Event(..), EventLambdaApplied(..), EventResultComputed(..)
    , ScopedVal(..)
    , whnfScopedVal, whnfThunk
    ) where

import           Control.Applicative (Applicative)
import           Control.Lens (at, use, traverse)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Either (EitherT(..), left)
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Data.Foldable (Foldable)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (Traversable)
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (ValHead, ValBody(..), Closure(..), Scope(..), emptyScope)
import           Lamdu.Eval.Val (ThunkId(..), thunkIdInt, ScopeId(..), scopeIdInt)
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V

data ThunkState pl
    = TSrc (ScopedVal pl)
    | TResult (ValHead pl)
    | TEvaluating -- BlackHoled in GHC
    deriving (Show, Functor, Foldable, Traversable)

data ScopedVal pl = ScopedVal
    { _srcScope :: !Scope
    , _srcExpr :: !(Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

data EventLambdaApplied pl = EventLambdaApplied
    { elaLam :: pl
    , elaParentId :: !ScopeId
    , elaId :: !ScopeId
    , elaArgument :: !ThunkId
    } deriving (Show, Functor, Foldable, Traversable)

data EventResultComputed pl = EventResultComputed
    { ercMThunkId :: !(Maybe ThunkId)
    , ercSource :: pl
    , ercScope :: !ScopeId
    , ercResult :: !(ValHead pl)
    } deriving (Show, Functor, Foldable, Traversable)

data Event pl
    = ELambdaApplied (EventLambdaApplied pl)
    | EResultComputed (EventResultComputed pl)
    deriving (Show, Functor, Foldable, Traversable)

data EvalActions m pl = EvalActions
    { _aReportEvent :: Event pl -> m ()
    , _aRunBuiltin :: Def.FFIName -> ThunkId -> EvalT pl m (ValHead pl)
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Def.Body (Val pl)))
    }

data EvalState m pl = EvalState
    { _esThunks :: !(Map ThunkId (ThunkState pl))
    , _esThunkCounter :: !ThunkId
    , _esScopeCounter :: !ScopeId
    , _esLoadedGlobals :: !(Map V.GlobalId (ValHead pl))
    , _esReader :: !(EvalActions m pl) -- This is ReaderT
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
Lens.makeLenses ''EvalState

freshThunkId :: Monad m => EvalT pl m ThunkId
freshThunkId =
    do
        thunkId <- use esThunkCounter
        esThunkCounter . thunkIdInt += 1
        return thunkId
    & liftState

report :: Monad m => Event pl -> EvalT pl m ()
report event =
    do
        rep <- liftState $ use $ esReader . aReportEvent
        rep event & lift

bindVar :: Monad m => pl -> V.Var -> ThunkId -> Scope -> EvalT pl m Scope
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
                        runBuiltin <- liftState $ use $ esReader . aRunBuiltin
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
        liftState $ esThunks . at thunkId .= Just (TSrc src)
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
    thunkState <- liftState $ use $ esThunks . at thunkId
    case thunkState of
        Just TEvaluating -> evalError "*INFINITE LOOP*"
        Just (TResult r) -> return r
        Just (TSrc thunkSrc) ->
            do
                res <- whnfScopedValInner (Just thunkId) thunkSrc
                liftState $ esThunks . at thunkId .= Just (TResult res)
                return res
        Nothing -> evalError $ "BUG: Referenced non-existing thunk " ++ show thunkId

loadGlobal :: Monad m => V.GlobalId -> EvalT pl m (ValHead pl)
loadGlobal g =
    do
        loaded <- liftState $ use (esLoadedGlobals . at g)
        case loaded of
            Just cached -> return cached
            Nothing -> do
                loader <- liftState $ use $ esReader . aLoadGlobal
                mLoadedGlobal <- lift $ loader g
                result <-
                    case mLoadedGlobal of
                    Nothing -> evalError $ "Global not found " ++ show g
                    Just (Def.BodyBuiltin (Def.Builtin name _t)) ->
                        return $ HBuiltin name
                    Just (Def.BodyExpr (Def.Expr expr _t)) ->
                        whnfScopedVal $ ScopedVal emptyScope expr
                liftState $ esLoadedGlobals . at g .= Just result
                return result

initialState :: EvalActions m pl -> EvalState m pl
initialState actions =
    EvalState
    { _esThunks = Map.empty
    , _esThunkCounter = ThunkId 0
    , _esScopeCounter = ScopeId 1
    , _esLoadedGlobals = Map.empty
    , _esReader = actions
    }
