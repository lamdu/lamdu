{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lamdu.Eval.Background
    ( Evaluator
    , Actions(..)
    , start, stop
    , pauseLoading, resumeLoading
    , getResults
    ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad ((>=>), void)
import           Control.Monad.Trans.Either (runEitherT)
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable (foldMap)
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval (EvalT)
import qualified Lamdu.Eval as Eval
import           Lamdu.Eval.Results (EvalResults(..))
import qualified Lamdu.Eval.Results as Results
import           Lamdu.Eval.Val (ValHead, ThunkId, ScopeId)
import qualified Lamdu.Eval.Val as EvalVal
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import           System.IO (stderr)

-- import           Control.Monad
-- import           Debug.Trace

data Actions pl = Actions
    { _aLoadGlobal :: V.GlobalId -> IO (Maybe (Def.Body (Val pl)))
    , _aRunBuiltin :: Def.FFIName -> ThunkId -> EvalT pl IO (ValHead pl)
    , _aReportUpdatesAvailable :: IO ()
    }

aLoadGlobal :: Lens' (Actions pl) (V.GlobalId -> IO (Maybe (Def.Body (Val pl))))
aLoadGlobal f Actions{..} = f _aLoadGlobal <&> \_aLoadGlobal -> Actions{..}

data Evaluator pl = Evaluator
    { eStateRef :: IORef (State pl)
    , eThreadId :: ThreadId
    , eLoadResumed :: MVar () -- taken when paused
    }

data Status
    = Running
    | Stoppped
    | Error
    | Finished

data State pl = State
    { _sStatus :: !Status
    , _sAppliesOfLam :: !(Map pl (Map ScopeId [(ScopeId, ThunkId)]))
      -- Maps of already-evaluated pl's/thunks
    , _sValHeadMap :: !(Map pl (Map ScopeId (ValHead pl)))
    , _sThunkMap :: !(Map ThunkId (ValHead pl))
    , _sDependencies :: !(Set pl, Set V.GlobalId)
    }

sAppliesOfLam :: Lens' (State pl) (Map pl (Map ScopeId [(ScopeId, ThunkId)]))
sAppliesOfLam f State{..} = f _sAppliesOfLam <&> \_sAppliesOfLam -> State{..}

sStatus :: Lens' (State pl) Status
sStatus f State{..} = f _sStatus <&> \_sStatus -> State{..}

sValHeadMap :: Lens' (State pl) (Map pl (Map ScopeId (ValHead pl)))
sValHeadMap f State{..} = f _sValHeadMap <&> \_sValHeadMap -> State{..}

sThunkMap :: Lens' (State pl) (Map ThunkId (ValHead pl))
sThunkMap f State{..} = f _sThunkMap <&> \_sThunkMap -> State{..}

sDependencies :: Lens' (State pl) (Set pl, Set V.GlobalId)
sDependencies f State{..} = f _sDependencies <&> \_sDependencies -> State{..}

initialState :: State pl
initialState = State
    { _sStatus = Running
    , _sAppliesOfLam = Map.empty
    , _sValHeadMap = Map.empty
    , _sThunkMap = Map.empty
    , _sDependencies = (Set.empty, Set.empty)
    }

writeStatus :: IORef (State pl) -> Status -> IO ()
writeStatus stateRef newStatus =
    atomicModifyIORef' stateRef $ \x -> (x & sStatus .~ newStatus, ())

setJust :: a -> Maybe a -> Maybe a
setJust x Nothing = Just x
setJust _ (Just _) = error "Conflicting values in setJust"

processEvent :: Ord pl => Eval.Event pl -> State pl -> State pl
processEvent (Eval.ELambdaApplied Eval.EventLambdaApplied{..}) state =
    state & sAppliesOfLam %~ Map.alter addApply elaLam
    where
        apply = Map.singleton elaParentId [(elaId, elaArgument)]
        addApply Nothing = Just apply
        addApply (Just x) = Just $ Map.unionWith (++) x apply
processEvent (Eval.EResultComputed Eval.EventResultComputed{..}) state =
    state
    & sValHeadMap %~ Map.alter (<> Just (Map.singleton ercScope ercResult)) ercSource
    & case ercMThunkId of
        Nothing -> id
        Just thunkId -> sThunkMap %~ Map.alter (setJust ercResult) thunkId

getDependencies :: Ord pl => V.GlobalId -> Maybe (Def.Body (Val pl)) -> (Set pl, Set V.GlobalId)
getDependencies globalId defBody =
    ( defBody ^. Lens._Just . Lens.traverse . Lens.traverse . Lens.to Set.singleton
    , Set.singleton globalId
    )

evalActions :: Ord pl => Actions pl -> IORef (State pl) -> Eval.EvalActions IO pl
evalActions actions stateRef =
    Eval.EvalActions
    { _aReportEvent = update . processEvent
    , _aRunBuiltin = _aRunBuiltin actions
    , _aLoadGlobal = loadGlobal
    }
    where
        loadGlobal globalId =
            do
                defBody <- (actions ^. aLoadGlobal) globalId
                modifyState (sDependencies <>~ getDependencies globalId defBody)
                return defBody
        modifyState f = atomicModifyIORef' stateRef (f <&> flip (,) ())
        update f =
            do
                modifyState f
                _aReportUpdatesAvailable actions

evalThread :: Ord pl => Actions pl -> IORef (State pl) -> Val pl -> IO ()
evalThread actions stateRef src =
    do
        result <-
            Eval.whnfScopedVal (Eval.ScopedVal EvalVal.emptyScope src)
            >>= force
            & Eval.runEvalT
            & runEitherT
            & (`evalStateT` Eval.initialState env)
        case result of
            Left e -> handleError e Error
            Right _ -> return ()
        writeStatus stateRef Finished
    `E.catch` \e@E.SomeException{..} -> handleError e Error
    where
        env = Eval.Env $ evalActions actions stateRef
        handleError e t =
            do
                BS8.hPutStrLn stderr $ "Background evaluator thread failed: " <> BS8.pack (show e)
                writeStatus stateRef t
        force = Lens.traverseOf_ EvalVal.children (Eval.whnfThunk >=> force)

results :: State pl -> EvalResults pl
results state =
    EvalResults
    { erExprValues =
        state ^. sValHeadMap
        <&> Lens.mapped %~ Results.derefValHead (state ^. sThunkMap)
        <&> Lens.mapped . Lens.mapped .~ ()
    , erAppliesOfLam =
        state ^. sAppliesOfLam
        <&> Lens.mapped . Lens.mapped . Lens._2 %~
            void . Results.derefThunkId (state ^. sThunkMap)
    }

getState :: Evaluator pl -> IO (State pl)
getState = readIORef . eStateRef

getResults :: Evaluator pl -> IO (EvalResults pl)
getResults evaluator = getState evaluator <&> results

withLock :: MVar () -> IO a -> IO a
withLock mvar action = withMVar mvar (const action)

start :: Ord pl => Actions pl -> Val pl -> IO (Evaluator pl)
start actions src =
    do
        stateRef <-
            initialState
            & sDependencies . _1 <>~ foldMap Set.singleton src
            & newIORef
        mvar <- newMVar ()
        let lockedActions = actions & aLoadGlobal %~ (withLock mvar .)
        newThreadId <- forkIOUnmasked $ evalThread lockedActions stateRef src
        return Evaluator
            { eStateRef = stateRef
            , eThreadId = newThreadId
            , eLoadResumed = mvar
            }

stop :: Evaluator pl -> IO ()
stop evaluator =
    do
        killThread (eThreadId evaluator)
        writeStatus (eStateRef evaluator) Stoppped

pauseLoading :: Evaluator pl -> IO (Set pl, Set V.GlobalId)
pauseLoading evaluator =
    do
        takeMVar (eLoadResumed evaluator)
        -- When pausing, dependency list stops being changed (only the
        -- now-paused loadGlobal may change it) so this is a coherent
        -- position to return it
        getState evaluator <&> (^. sDependencies)

resumeLoading :: Evaluator pl -> IO ()
resumeLoading = (`putMVar` ()) . eLoadResumed
