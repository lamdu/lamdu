{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Lamdu.Eval
    ( EvalT(..), evalError
    , EvalState, initialState
    , ask
    , EvalActions(..)
    , Env(..), eEvalActions
    , Event(..), EventLambdaApplied(..), EventResultComputed(..)
    , ScopedVal(..)
    , whnfScopedVal, whnfThunk
    , asThunk
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens (at, use, traverse)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void, join)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Either (EitherT(..), left)
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Data.Foldable (Foldable)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (Traversable)
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (ThunkId(..), thunkIdInt, ScopeId(..), scopeIdInt)
import           Lamdu.Eval.Val (ValHead, ValBody(..), Closure(..), Scope(..), emptyScope)
import qualified Lamdu.Eval.Val as EvalVal
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

newtype Env m pl = Env
    { _eEvalActions :: EvalActions m pl
    }

data EvalState m pl = EvalState
    { _esThunks :: !(Map ThunkId (ThunkState pl))
    , _esThunkCounter :: !ThunkId
    , _esScopeCounter :: !ScopeId
    , _esLoadedGlobals :: !(Map V.GlobalId (ValHead pl))
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
        rep <- ask <&> (^. eEvalActions . aReportEvent)
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

whnfApplyThunked :: Monad m => ValHead pl -> ThunkId -> EvalT pl m (ValHead pl)
whnfApplyThunked func argThunk =
    case func of
    HFunc (Closure outerScope (V.Lam var body) lamPl) ->
        do
            innerScope <- bindVar lamPl var argThunk outerScope
            whnfScopedVal (ScopedVal innerScope body)
    HBuiltin ffiname ->
        do
            runBuiltin <- ask <&> (^. eEvalActions . aRunBuiltin)
            runBuiltin ffiname argThunk
    HCase (V.Case caseTag handlerThunk restThunk) ->
        do
            handlerFunc <- whnfThunk handlerThunk
            HInject (V.Inject sumTag valThunk) <- whnfThunk argThunk
            if caseTag == sumTag
                then whnfApplyThunked handlerFunc valThunk
                else
                do
                    rest <- whnfThunk restThunk
                    whnfApplyThunked rest argThunk
    _ -> evalError $ "Apply on non function: " ++ funcStr
        where
            funcStr = func & EvalVal.children .~ () & EvalVal.payloads .~ () & show

makeThunk :: Monad m => ThunkState pl -> EvalT pl m ThunkId
makeThunk src =
    do
        thunkId <- freshThunkId
        liftState $ esThunks . at thunkId .= Just src
        return thunkId

whnfApply :: Monad m => V.Apply (ScopedVal pl) -> EvalT pl m (ValHead pl)
whnfApply (V.Apply func arg) =
    whnfApplyThunked <$> whnfScopedVal func <*> makeThunk (TSrc arg) & join

whnfScopedValInner :: Monad m => Maybe ThunkId -> ScopedVal pl -> EvalT pl m (ValHead pl)
whnfScopedValInner mThunkId (ScopedVal scope expr) =
    reportResultComputed =<<
    case expr ^. V.body of
    V.BAbs lam -> return $ HFunc $ Closure scope lam (expr ^. V.payload)
    V.BApp apply -> apply <&> ScopedVal scope & whnfApply
    V.BGetField (V.GetField recordExpr tag) ->
        do
            record <- whnfScopedVal $ ScopedVal scope recordExpr
            whnfGetField $ V.GetField record tag
    V.BInject    inject    -> inject    & traverse %%~ thunk <&> HInject
    V.BRecExtend recExtend -> recExtend & traverse %%~ thunk <&> HRecExtend
    V.BCase      case_     -> case_     & traverse %%~ thunk <&> HCase
    V.BLeaf (V.LGlobal global) -> loadGlobal global
    V.BLeaf (V.LVar var) ->
        case scope ^. scopeMap . at var of
        Nothing -> evalError $ "Variable out of scope: " ++ show var
        Just thunkId -> whnfThunk thunkId
    V.BLeaf V.LRecEmpty -> return HRecEmpty
    V.BLeaf V.LAbsurd -> return HAbsurd
    V.BLeaf (V.LLiteralInteger i) -> HInteger i & return
    V.BLeaf V.LHole -> evalError "Hole"
    V.BFromNom (V.Nom _ v) -> ScopedVal scope v & whnfScopedValInner Nothing
    V.BToNom (V.Nom _ v) -> ScopedVal scope v & whnfScopedValInner Nothing
    where
        thunk = makeThunk . TSrc . ScopedVal scope
        reportResultComputed result =
            do
                EventResultComputed mThunkId (expr ^. V.payload) (scope ^. scopeId) result
                    & EResultComputed & report
                return result

whnfScopedVal :: Monad m => ScopedVal pl -> EvalT pl m (ValHead pl)
whnfScopedVal = whnfScopedValInner Nothing

whnfRecordShape :: Monad m => ThunkId -> EvalT pl m ()
whnfRecordShape restThunk =
    whnfThunk restThunk >>= go
    where
        go HRecEmpty = return ()
        go (HRecExtend (V.RecExtend _ _ newRestThunk)) = whnfRecordShape newRestThunk
        go x = error $ "RecExtend of non-record: " ++ show (void x)

whnfGetField :: Monad m => V.GetField (ValHead pl) -> EvalT pl m (ValHead pl)
whnfGetField (V.GetField (HRecExtend (V.RecExtend tag val restThunk)) searchTag)
    | searchTag == tag =
          do
              -- To avoid artifacts of RecExtend ordering, force the
              -- entire record shape rather than forcing the prefix we
              -- depend on:
              whnfRecordShape restThunk
              whnfThunk val
    | otherwise =
        whnfThunk restThunk
        <&> (`V.GetField` searchTag)
        >>= whnfGetField
whnfGetField (V.GetField val _) =
    evalError $ "GetField of value without the field " ++ show (void val)

asThunk :: Monad m => ValHead pl -> EvalT pl m ThunkId
asThunk = makeThunk . TResult

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
                loader <- ask <&> (^. eEvalActions . aLoadGlobal)
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

initialState :: Env m pl -> EvalState m pl
initialState env =
    EvalState
    { _esThunks = Map.empty
    , _esThunkCounter = ThunkId 0
    , _esScopeCounter = ScopeId 1
    , _esLoadedGlobals = Map.empty
    , _esReader = env
    }
