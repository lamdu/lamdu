{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Expr.Eval
    ( EvalT(..)
    , EvalState, EvalActions(..), initialState
    , evalError
    , ScopedVal(..), ValHead(..), ThunkId, Closure
    , Scope, emptyScope
    , whnfSrc, whnfThunk
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
import Lamdu.Expr.Val (Val)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Expr.Val as V

type ThunkId = Int

newtype Scope = Scope
    { _scopeMap :: Map V.Var ThunkId
    } deriving (Show)

emptyScope :: Scope
emptyScope = Scope Map.empty

data Closure pl = Closure
    { _cOuterScope :: Scope
    , _cLam :: V.Lam (Val pl)
    } deriving (Functor, Show)

data ValHead pl
    = HFunc (Closure pl)
    | HRecExtend (V.RecExtend ThunkId)
    | HRecEmpty
    | HLiteralInteger Integer
    | HBuiltin FFIName
    deriving (Functor, Show)

data ScopedVal pl = ScopedVal
    { _srcScope :: Scope
    , _srcExpr :: Val pl
    } deriving (Functor, Show)

data ThunkState pl
    = TSrc (ScopedVal pl)
    | TResult (ValHead pl)
    | TEvaluating -- BlackHoled in GHC
    deriving (Functor, Show)


data EvalActions m pl = EvalActions
    { _aLogProgress :: ScopedVal pl -> ValHead pl -> m ()
    , _aRunBuiltin :: FFIName -> ThunkId -> EvalT pl m (ValHead pl)
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Either FFIName (Val pl)))
    }

data EvalState m pl = EvalState
    { _esThunks :: Map ThunkId (ThunkState pl)
    , _esThunkCounter :: ThunkId
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

evalError :: Monad m => String -> EvalT pl m a
evalError = EvalT . left

logProgress :: Monad m => ScopedVal pl -> ValHead pl -> EvalT pl m (ValHead pl)
logProgress src result =
    do
        lp <- esReader . aLogProgress & use
        lift $ lp src result
        return result

whnfSrc :: Monad m => ScopedVal pl -> EvalT pl m (ValHead pl)
whnfSrc src@(ScopedVal scope expr) =
    logProgress src =<<
    case expr ^. V.body of
    V.BAbs lam -> return $ HFunc $ Closure scope lam
    V.BApp (V.Apply funcExpr argExpr) ->
        do
            func <- whnfSrc $ ScopedVal scope funcExpr
            argThunk <- makeThunk (ScopedVal scope argExpr)
            case func of
                HFunc (Closure outerScope (V.Lam var body)) ->
                    whnfSrc (ScopedVal innerScope body)
                    where
                        innerScope = outerScope & scopeMap . at var .~ Just argThunk
                HBuiltin ffiname ->
                    do
                        runBuiltin <- use $ esReader . aRunBuiltin
                        runBuiltin ffiname argThunk
                _ -> evalError "Apply on non function"
    V.BGetField (V.GetField recordExpr tag) ->
        do
            record <- whnfSrc $ ScopedVal scope recordExpr
            whnfGetField $ V.GetField record tag
    V.BRecExtend recExtend ->
        recExtend & traverse %%~ makeThunk . ScopedVal scope <&> HRecExtend
    V.BLeaf (V.LGlobal global) -> loadGlobal global
    V.BLeaf (V.LVar var) ->
        case scope ^. scopeMap . at var of
        Nothing -> evalError $ "Variable out of scope: " ++ show var
        Just thunkId -> whnfThunk thunkId
    V.BLeaf V.LRecEmpty -> return HRecEmpty
    V.BLeaf (V.LLiteralInteger i) -> HLiteralInteger i & return
    V.BLeaf V.LHole -> evalError "Hole"

makeThunk :: Monad m => ScopedVal pl -> EvalT pl m ThunkId
makeThunk src =
    do
        thunkId <- use esThunkCounter
        esThunkCounter += 1
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
        Just (TSrc thunkSrc) -> whnfSrc thunkSrc >>= newResult
        Nothing -> evalError $ "BUG: Referenced non-existing thunk " ++ show thunkId
    where
        newResult res =
            do
                esThunks . at thunkId .= Just (TResult res)
                return res

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
                    Just (Right expr) -> whnfSrc $ ScopedVal emptyScope expr
                esLoadedGlobals . at g .= Just result
                return result

initialState :: EvalActions m pl -> EvalState m pl
initialState actions =
    EvalState
    { _esThunks = Map.empty
    , _esThunkCounter = 0
    , _esLoadedGlobals = Map.empty
    , _esReader = actions
    }
