{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Expr.Eval
    ( EvalT(..)
    , EvalState, EvalActions(..), initialState
    , evalError
    , ThunkSrc(..), ValHead(..), ThunkId, Closure, ClosureId, outermostClosure
    , whnf, whnfVar
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

type ClosureId = Int
type AnonThunkId = Int

data LocalVar = LocalVar
    { _lClosure :: ClosureId
    , _lVar :: V.Var
    } deriving (Eq, Ord, Show)

data ThunkId
    = TGlobal V.GlobalId
    | TLocalVar LocalVar
    | TAnon AnonThunkId
    deriving (Eq, Ord, Show)

data ThunkSrc pl = ThunkSrc
    { _srcMClosure :: ClosureId
    , _srcExpr :: Val pl
    } deriving (Functor, Show)

data Closure pl = Closure
    { _cOuterScope :: ClosureId
    , _cLam :: V.Lam (Val pl)
    } deriving (Functor, Show)

data ValHead pl
    = HAbs (Closure pl)
    | HRecExtend (V.RecExtend ThunkId)
    | HRecEmpty
    | HLiteralInteger Integer
    | HBuiltin FFIName
    deriving (Functor, Show)

data ThunkState srcPayload
    = TSrc (ThunkSrc srcPayload)
    | TResult (ValHead srcPayload)
    | TEvaluating -- BlackHoled in GHC
    deriving (Functor, Show)

data EvalActions m pl = EvalActions
    { _aLogProgress :: ThunkSrc pl -> ValHead pl -> m ()
    , _aRunBuiltin :: FFIName -> ThunkId -> EvalT pl m (ValHead pl)
    , _aLoadGlobal :: V.GlobalId -> m (Maybe (Either FFIName (Val pl)))
    }

data EvalState m srcPayload = EvalState
    { _esThunks :: Map ThunkId (ThunkState srcPayload)
    , _esThunkCounter :: AnonThunkId
    , _esClosureParent :: Map ClosureId ClosureId
    , _esClosureCounter :: ClosureId
    , -- esReader inside state used instead of adding a ReaderT
      _esReader :: EvalActions m srcPayload
    }

newtype EvalT srcPayload m a =
    EvalT { runEvalT :: EitherT String (StateT (EvalState m srcPayload) m) a }
    deriving (Functor, Applicative, Monad, MonadState (EvalState m srcPayload))

instance MonadTrans (EvalT pl) where
    lift = EvalT . lift . lift

Lens.makeLenses ''Closure
Lens.makeLenses ''EvalActions
Lens.makeLenses ''EvalState
Lens.makePrisms ''ThunkId

evalError :: Monad m => String -> EvalT pl m a
evalError = EvalT . left

whnf :: Monad m => ThunkSrc pl -> EvalT pl m (ValHead pl)
whnf src =
    do
        result <- calcWhnf src
        logProgress <- use $ esReader . aLogProgress
        lift $ logProgress src result
        return result

calcWhnf :: Monad m => ThunkSrc pl -> EvalT pl m (ValHead pl)
calcWhnf (ThunkSrc closureId expr) =
    case expr ^. V.body of
    V.BAbs lam -> return $ HAbs $ Closure closureId lam
    V.BApp (V.Apply funcExpr argExpr) ->
        do
            func <- whnf $ ThunkSrc closureId funcExpr
            case func of
                HAbs closure -> whnfApplyLam closure (ThunkSrc closureId argExpr)
                HBuiltin ffiname ->
                    do
                        runBuiltin <- use $ esReader . aRunBuiltin
                        argThunk <- makeThunk closureId argExpr
                        runBuiltin ffiname argThunk
                _ -> evalError "Apply on non function"
    V.BGetField (V.GetField recordExpr tag) ->
        do
            record <- whnf $ ThunkSrc closureId recordExpr
            whnfGetField $ V.GetField record tag
    V.BRecExtend recExtend ->
        recExtend & traverse %%~ makeThunk closureId <&> HRecExtend
    V.BLeaf (V.LGlobal global) -> whnfVar $ TGlobal global
    V.BLeaf (V.LVar var) -> whnfVar $ TLocalVar $ LocalVar closureId var
    V.BLeaf V.LRecEmpty -> return HRecEmpty
    V.BLeaf (V.LLiteralInteger i) -> HLiteralInteger i & return
    V.BLeaf V.LHole -> evalError "Hole"

whnfApplyLam :: Monad m => Closure pl -> ThunkSrc pl -> EvalT pl m (ValHead pl)
whnfApplyLam func arg =
    do
        closureId <- use esClosureCounter
        esClosureCounter += 1
        esClosureParent . at closureId .= Just (func ^. cOuterScope)
        let var = LocalVar closureId $ func ^. cLam . V.lamParamId
        esThunks . at (TLocalVar var) .= Just (TSrc arg)
        whnf $ ThunkSrc closureId (func ^. cLam . V.lamResult)

makeThunk :: Monad m => ClosureId -> Val pl -> EvalT pl m ThunkId
makeThunk closureId expr =
    do
        thunkId <- use esThunkCounter <&> TAnon
        esThunkCounter += 1
        esThunks . at thunkId .= Just (TSrc (ThunkSrc closureId expr))
        return thunkId

whnfGetField :: Monad m => V.GetField (ValHead pl) -> EvalT pl m (ValHead pl)
whnfGetField (V.GetField (HRecExtend (V.RecExtend otherTag otherVal restRef)) tag)
    | tag == otherTag = whnfVar otherVal
    | otherwise =
        whnfVar restRef
        <&> (`V.GetField` tag)
        >>= whnfGetField
whnfGetField (V.GetField val _) =
    evalError $ "GetField of value without the field " ++ show (void val)

whnfVar :: Monad m => ThunkId -> EvalT pl m (ValHead pl)
whnfVar var = do
    thunkState <- use $ esThunks . at var
    case thunkState of
        Just TEvaluating -> evalError "TODO: recursive whnfVar"
        Just (TResult r) -> return r
        Just (TSrc thunkSrc) -> whnf thunkSrc >>= newResult
        Nothing ->
            case var of
            TGlobal g -> loadGlobal g >>= newResult
            TLocalVar (LocalVar closureId lvar) ->
                do
                    closureParents <- use $ esClosureParent
                    case closureParents ^. at closureId of
                        Just parentClosure -> whnfVar $ TLocalVar (LocalVar parentClosure lvar)
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
            Just (Right expr) -> whnf $ ThunkSrc outermostClosure expr

initialState :: EvalActions m srcPayload -> EvalState m srcPayload
initialState actions =
    EvalState
    { _esThunks = Map.empty
    , _esThunkCounter = 0
    , _esClosureParent = Map.empty
    , _esClosureCounter = 1
    , _esReader = actions
    }

outermostClosure :: ClosureId
outermostClosure = 0
