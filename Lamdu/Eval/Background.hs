{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Lamdu.Eval.Background
    ( Evaluator
    , Actions(..), aLoadGlobal, aReportUpdatesAvailable, aCompleted
    , start, stop
    , pauseLoading, resumeLoading
    , getResults
    ) where

import           Prelude.Compat

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join)
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Lamdu.Builtins as Builtins
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval as Eval
import           Lamdu.Eval.Results (EvalResults(..))
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Eval.Val (Val, ScopeId)
import qualified Lamdu.Eval.Val as EvalVal
import qualified Lamdu.Expr.Val as V
import           System.IO (stderr)

data Actions srcId = Actions
    { _aLoadGlobal :: V.Var -> IO (Maybe (Def.Body (V.Val srcId)))
    , _aReportUpdatesAvailable :: IO ()
    , _aCompleted :: Either E.SomeException (Val srcId) -> IO ()
    }

Lens.makeLenses ''Actions

data State srcId = State
    { _sAppliesOfLam :: !(Map srcId (Map ScopeId [(ScopeId, Val srcId)]))
      -- Maps of already-evaluated srcId's/thunks
    , _sValMap :: !(Map srcId (Map ScopeId (Val srcId)))
    , _sDependencies :: !(Set srcId, Set V.Var)
    }

Lens.makeLenses ''State

data Evaluator srcId = Evaluator
    { eStateRef :: IORef (State srcId)
    , eThreadId :: ThreadId
    , eLoadResumed :: MVar () -- taken when paused
    }

emptyState :: State srcId
emptyState = State
    { _sAppliesOfLam = Map.empty
    , _sValMap = Map.empty
    , _sDependencies = (Set.empty, Set.empty)
    }

processEvent :: Ord srcId => Eval.Event srcId -> State srcId -> State srcId
processEvent (Eval.ELambdaApplied Eval.EventLambdaApplied{..}) state =
    state & sAppliesOfLam %~ Map.alter addApply elaLam
    where
        apply = Map.singleton elaParentId [(elaId, elaArgument)]
        addApply Nothing = Just apply
        addApply (Just x) = Just $ Map.unionWith (++) x apply
processEvent (Eval.EResultComputed Eval.EventResultComputed{..}) state =
    state
    & sValMap %~ Map.alter (<> Just (Map.singleton ercScope ercResult)) ercSource

getDependencies :: Ord srcId => V.Var -> Maybe (Def.Body (V.Val srcId)) -> (Set srcId, Set V.Var)
getDependencies globalId defBody =
    ( defBody ^. Lens._Just . Lens.traverse . Lens.traverse . Lens.to Set.singleton
    , Set.singleton globalId
    )

evalActions :: Ord srcId => Actions srcId -> IORef (State srcId) -> Eval.EvalActions IO srcId
evalActions actions stateRef =
    Eval.EvalActions
    { _aReportEvent = update . processEvent
    , _aRunBuiltin = Builtins.eval
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

evalThread ::
    Ord srcId => Actions srcId -> IORef (State srcId) -> V.Val srcId -> IO ()
evalThread actions stateRef src =
    ( ( Eval.evalScopedVal (Eval.ScopedVal EvalVal.emptyScope src)
        & Eval.runEvalT
        & (`evalStateT` Eval.initialState env)
        <&> Right
        <&> actions ^. aCompleted
      )
      `E.catch` (\e -> case e of
          E.ThreadKilled -> return (return ())
          _ -> E.throwIO e)
      `E.catch` (\e@E.SomeException{} ->
      do
          BS8.hPutStrLn stderr $ "Background evaluator thread failed: " <> BS8.pack (show e)
          Left e & actions ^. aCompleted & return)
    ) & join
    where
        env = Eval.Env $ evalActions actions stateRef

results :: State srcId -> EvalResults srcId
results state =
    EvalResults
    { _erExprValues = state ^. sValMap <&> Lens.mapped %~ ER.fromEval
    , _erAppliesOfLam =
        state ^. sAppliesOfLam
        <&> Lens.mapped . Lens.mapped . Lens._2 %~ ER.fromEval
    }

getState :: Evaluator srcId -> IO (State srcId)
getState = readIORef . eStateRef

getResults :: Evaluator srcId -> IO (EvalResults srcId)
getResults evaluator = getState evaluator <&> results

withLock :: MVar () -> IO a -> IO a
withLock mvar action = withMVar mvar (const action)

start :: Ord srcId => Actions srcId -> V.Val srcId -> IO (Evaluator srcId)
start actions src =
    do
        stateRef <-
            emptyState
            & sDependencies . _1 <>~ foldMap Set.singleton src
            & newIORef
        mvar <- newMVar ()
        let lockedActions = actions & aLoadGlobal %~ (withLock mvar .)
        newThreadId <- evalThread lockedActions stateRef src & forkIOUnmasked
        return Evaluator
            { eStateRef = stateRef
            , eThreadId = newThreadId
            , eLoadResumed = mvar
            }

stop :: Evaluator srcId -> IO ()
stop = killThread . eThreadId

pauseLoading :: Evaluator srcId -> IO (Set srcId, Set V.Var)
pauseLoading evaluator =
    do
        takeMVar (eLoadResumed evaluator)
        -- When pausing, dependency list stops being changed (only the
        -- now-paused loadGlobal may change it) so this is a coherent
        -- position to return it
        getState evaluator <&> (^. sDependencies)

resumeLoading :: Evaluator srcId -> IO ()
resumeLoading = (`putMVar` ()) . eLoadResumed
