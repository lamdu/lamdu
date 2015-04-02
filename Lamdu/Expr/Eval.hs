{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Expr.Eval
    ( EvalT(..)
    , EvalState, EvalActions(..), initialState
    , evalError
    , ThunkSrc(..), ValHead(..), ThunkId, Closure, ScopeId, outermostScope
    , whnfSrc, whnfThunk
    ) where

import Control.Applicative (Applicative)
import Control.Lens (at, use, ix, traverse)
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

type ScopeId = Int
type AnonThunkId = Int

data LocalVar = LocalVar
    { _lScope :: ScopeId
    , _lVar :: V.Var
    } deriving (Eq, Ord, Show)

data ThunkId
    = TGlobal V.GlobalId
    | TLocalVar LocalVar
    | TAnon AnonThunkId
    deriving (Eq, Ord, Show)

data ThunkSrc pl = ThunkSrc
    { _srcScope :: ScopeId
    , _srcExpr :: Val pl
    } deriving (Functor, Show)

data Closure pl = Closure
    { _cOuterScope :: ScopeId
    , _cLam :: V.Lam (Val pl)
    } deriving (Functor, Show)

data ValHead pl
    = HFunc (Closure pl)
    | HRecExtend (V.RecExtend ThunkId)
    | HRecEmpty
    | HLiteralInteger Integer
    | HBuiltin FFIName
    deriving (Functor, Show)

data ThunkState pl
    = TSrc (ThunkSrc pl)
    | TResult (ValHead pl)
    | TEvaluating -- BlackHoled in GHC
    deriving (Functor, Show)

data EvalActions m pl = EvalActions
    { _aLogProgress :: ThunkSrc pl -> ValHead pl -> m ()
    , _aRunBuiltin :: FFIName -> ThunkId -> EvalT pl m (ValHead pl)
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Either FFIName (Val pl)))
    }

data EvalState m pl = EvalState
    { _esThunks :: Map ThunkId (ThunkState pl)
    , _esThunkCounter :: AnonThunkId
    , _esScopeParents :: Map ScopeId ScopeId
    , _esScopeCounter :: ScopeId
    , -- esReader inside state used instead of adding a ReaderT
      _esReader :: EvalActions m pl
    }

newtype EvalT pl m a =
    EvalT { runEvalT :: EitherT String (StateT (EvalState m pl) m) a }
    deriving (Functor, Applicative, Monad, MonadState (EvalState m pl))

instance MonadTrans (EvalT pl) where
    lift = EvalT . lift . lift

Lens.makeLenses ''Closure
Lens.makeLenses ''EvalActions
Lens.makeLenses ''EvalState
Lens.makePrisms ''ThunkId

evalError :: Monad m => String -> EvalT pl m a
evalError = EvalT . left

logProgress :: Monad m => ThunkSrc pl -> ValHead pl -> EvalT pl m (ValHead pl)
logProgress src result =
    do
        lp <- esReader . aLogProgress & use
        lift $ lp src result
        return result

whnfSrc :: Monad m => ThunkSrc pl -> EvalT pl m (ValHead pl)
whnfSrc src@(ThunkSrc scopeId expr) =
    logProgress src =<<
    case expr ^. V.body of
    V.BAbs lam -> return $ HFunc $ Closure scopeId lam
    V.BApp (V.Apply funcExpr argExpr) ->
        do
            func <- whnfSrc $ ThunkSrc scopeId funcExpr
            case func of
                HFunc closure -> whnfRedex closure (ThunkSrc scopeId argExpr)
                HBuiltin ffiname ->
                    do
                        runBuiltin <- use $ esReader . aRunBuiltin
                        argThunk <- makeThunk (ThunkSrc scopeId argExpr)
                        runBuiltin ffiname argThunk
                _ -> evalError "Apply on non function"
    V.BGetField (V.GetField recordExpr tag) ->
        do
            record <- whnfSrc $ ThunkSrc scopeId recordExpr
            whnfGetField $ V.GetField record tag
    V.BRecExtend recExtend ->
        recExtend & traverse %%~ makeThunk . ThunkSrc scopeId <&> HRecExtend
    V.BLeaf (V.LGlobal global) -> whnfThunk $ TGlobal global
    V.BLeaf (V.LVar var) -> whnfThunk $ TLocalVar $ LocalVar scopeId var
    V.BLeaf V.LRecEmpty -> return HRecEmpty
    V.BLeaf (V.LLiteralInteger i) -> HLiteralInteger i & return
    V.BLeaf V.LHole -> evalError "Hole"

whnfRedex :: Monad m => Closure pl -> ThunkSrc pl -> EvalT pl m (ValHead pl)
whnfRedex func arg =
    do
        scopeId <- use esScopeCounter
        esScopeCounter += 1
        esScopeParents . at scopeId .= Just (func ^. cOuterScope)
        let var = LocalVar scopeId $ func ^. cLam . V.lamParamId
        esThunks . at (TLocalVar var) .= Just (TSrc arg)
        whnfSrc $ ThunkSrc scopeId (func ^. cLam . V.lamResult)

makeThunk :: Monad m => ThunkSrc pl -> EvalT pl m ThunkId
makeThunk src =
    do
        thunkId <- use esThunkCounter <&> TAnon
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
whnfThunk var = do
    thunkState <- use $ esThunks . at var
    case thunkState of
        Just TEvaluating -> evalError "*INFINITE LOOP*"
        Just (TResult r) -> return r
        Just (TSrc thunkSrc) -> whnfSrc thunkSrc >>= newResult
        Nothing ->
            case var of
            TGlobal g -> loadGlobal g >>= newResult
            TLocalVar (LocalVar scopeId lvar) ->
                do
                    scopeParents <- use $ esScopeParents
                    case scopeParents ^. at scopeId of
                        Just scopeParent -> whnfThunk $ TLocalVar (LocalVar scopeParent lvar)
                        Nothing -> evalError $ "Var not found " ++ show var
            TAnon i -> evalError $ "BUG: Referenced non-existing thunk " ++ show i
    where
        newResult res =
            do
                esThunks . ix var .= TResult res
                return res

loadGlobal :: Monad m => V.GlobalId -> EvalT pl m (ValHead pl)
loadGlobal g =
    do
        loader <- use $ esReader . aLoadGlobal
        res <- lift $ loader g
        case res of
            Nothing -> evalError $ "Global not found " ++ show g
            Just (Left name) -> return $ HBuiltin name
            Just (Right expr) -> whnfSrc $ ThunkSrc outermostScope expr

initialState :: EvalActions m pl -> EvalState m pl
initialState actions =
    EvalState
    { _esThunks = Map.empty
    , _esThunkCounter = 0
    , _esScopeParents = Map.empty
    , _esScopeCounter = 1
    , _esReader = actions
    }

outermostScope :: ScopeId
outermostScope = 0
