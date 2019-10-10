{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Lamdu.Eval.Manager
    ( Evaluator
    , NewParams(..), new
    , start, stop, executeReplIOProcess
    , getResults
    , runTransactionAndMaybeRestartEvaluator
    ) where

import           Control.Concurrent.Extended (ThreadId, killThread, runAfter)
import           Control.Concurrent.MVar
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.IORef.Extended
import qualified Data.Monoid as Monoid
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import           Hyper
import           Hyper.Type.Functor (_F)
import           Lamdu.Calc.Term (Val)
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.JS as Eval
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (toUUID)
import           Lamdu.VersionControl (getVersion)
import qualified Lamdu.VersionControl as VersionControl
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import qualified Revision.Deltum.Rev.Change as Change
import qualified Revision.Deltum.Rev.Version as Version
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data BGEvaluator = NotStarted | Started (Eval.Evaluator (ValI ViewM))

Lens.makePrisms ''BGEvaluator

data NewParams = NewParams
    { resultsUpdated :: IO ()
    -- ^ Callback for notifying that new evaluation results are available.
    , dbMVar :: MVar (Maybe (Transaction.Store DbM))
    , jsDebugPaths :: Eval.JSDebugPaths FilePath
    }

data Evaluator = Evaluator
    { eParams :: NewParams
    , eEvaluatorRef :: IORef BGEvaluator
    , ePrevResultsRef :: IORef (EvalResults (ValI ViewM))
    , eCancelTimerRef :: IORef (Maybe ThreadId)
    }

new :: NewParams -> IO Evaluator
new params =
    Evaluator params
    <$> newIORef NotStarted
    <*> newIORef EvalResults.empty
    <*> newIORef Nothing

withDb :: MVar (Maybe (Transaction.Store DbM)) -> (Transaction.Store DbM -> IO a) -> IO a
withDb mvar action =
    withMVar mvar $ \case
    Nothing -> error "Trying to use DB when it is already gone"
    Just db -> action db

runViewTransactionInIO :: MVar (Maybe (Transaction.Store DbM)) -> T ViewM a -> IO a
runViewTransactionInIO dbM trans =
    withDb dbM $ \db ->
    DbLayout.runDbTransaction db (VersionControl.runAction trans)

getLatestResults :: Evaluator -> IO (EvalResults (ValI ViewM))
getLatestResults evaluator =
    readIORef (eEvaluatorRef evaluator) <&> (^? _Started)
    >>= maybe (pure EvalResults.empty) Eval.getResults

getResults :: Evaluator -> IO (CurAndPrev (EvalResults (ValI ViewM)))
getResults evaluator =
    do
        res <- getLatestResults evaluator
        prevResults <- readIORef (ePrevResultsRef evaluator)
        pure CurAndPrev { _prev = prevResults, _current = res }

eDb :: Evaluator -> MVar (Maybe (Transaction.Store DbM))
eDb = dbMVar . eParams

loadDef ::
    Evaluator -> DefI ViewM ->
    IO (Def.Definition (Val (ExprIRef.ValP ViewM)) (DefI ViewM))
loadDef evaluator = runViewTransactionInIO (eDb evaluator) . Load.def

evalActions :: Evaluator -> Eval.Actions (ValI ViewM)
evalActions evaluator =
    Eval.Actions
    { Eval._aLoadGlobal = loadGlobal
    , Eval._aReportUpdatesAvailable =
      do
          res <- getLatestResults evaluator
          when (Lens.has (EvalResults.erCompleted . Lens._Just) res) $
              writeIORef (ePrevResultsRef evaluator) EvalResults.empty
          resultsUpdated (eParams evaluator)
    , Eval._aJSDebugPaths = jsDebugPaths (eParams evaluator)
    }
    where
        loadGlobal globalId =
            ExprIRef.defI globalId
            & loadDef evaluator
            <&> Def.defBody . Lens.mapped . Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ Property.value
            <&> Lens.mapped .~ ()

replIRef :: IRef ViewM (Def.Expr (ValI ViewM))
replIRef = DbLayout.repl DbLayout.codeIRefs

startBG ::
    Eval.Actions (ValI m) -> Def.Expr (Val (ValI m)) ->
    IO (Eval.Evaluator (ValI m))
startBG = Eval.start toUUID ((_F #) . IRef.unsafeFromUUID)

start :: Evaluator -> IO ()
start evaluator =
    readIORef (eEvaluatorRef evaluator)
    >>= \case
    Started {} -> pure () -- already started
    NotStarted ->
        DbLayout.repl DbLayout.codeAnchors
        & Load.defExpr
        & runViewTransactionInIO (eDb evaluator)
        <&> Lens.mapped . Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ Property.value
        >>= startBG
            (evalActions evaluator) <&> Started
        >>= writeIORef (eEvaluatorRef evaluator)

onEvaluator :: (Eval.Evaluator (ValI ViewM) -> IO ()) -> Evaluator -> IO ()
onEvaluator action evaluator =
    readIORef (eEvaluatorRef evaluator)
    <&> (^? _Started)
    >>= traverse_ action

stop :: Evaluator -> IO ()
stop evaluator =
    do
        onEvaluator Eval.stop evaluator
        writeIORef (eEvaluatorRef evaluator) NotStarted
        writeIORef (ePrevResultsRef evaluator) EvalResults.empty

executeReplIOProcess :: Evaluator -> IO ()
executeReplIOProcess = onEvaluator Eval.executeReplIOProcess

sumDependency :: Eval.Dependencies (ValI m) -> Set UUID
sumDependency (Eval.Dependencies subexprs globals) =
    mconcat
    [ Set.map toUUID subexprs
    , Set.map (toUUID . ExprIRef.defI) globals
    ]

runTransactionAndMaybeRestartEvaluator :: Evaluator -> T DbM a -> IO a
runTransactionAndMaybeRestartEvaluator evaluator transaction =
    readIORef (eEvaluatorRef evaluator)
    >>= \case
    NotStarted -> runTrans transaction
    Started eval ->
        Eval.whilePaused eval $
        \rawDependencies ->
        do
            let dependencies =
                    sumDependency rawDependencies & Set.insert (toUUID replIRef)
            (dependencyChanged, result) <-
                do
                    (oldVersion, result, newVersion) <-
                        (,,) <$> getVersion <*> transaction <*> getVersion
                    let checkDependencyChange versionData =
                            Version.changes versionData
                            <&> Change.objectKey <&> (`Set.member` dependencies)
                            <&> Monoid.Any & mconcat & pure
                    Monoid.Any dependencyChanged <-
                        Version.walk checkDependencyChange checkDependencyChange oldVersion newVersion
                    pure (dependencyChanged, result)
                & runTrans
            when dependencyChanged $
                do
                    prevResults <- getLatestResults evaluator
                    stop evaluator
                    setCancelTimer evaluator
                    atomicModifyIORef_ (ePrevResultsRef evaluator) (const prevResults)
                    start evaluator
            pure result
    where
        runTrans trans =
            withDb (eDb evaluator) $ \db -> DbLayout.runDbTransaction db trans

setCancelTimer :: Evaluator -> IO ()
setCancelTimer evaluator =
    do
        newCancelTimer <-
            runAfter 5000000 $ -- 5 seconds
            do
                atomicModifyIORef (ePrevResultsRef evaluator)
                    ((, ()) . const EvalResults.empty)
                resultsUpdated (eParams evaluator)
        atomicModifyIORef (eCancelTimerRef evaluator)
            (Just newCancelTimer,)
            >>= Lens.traverseOf_ Lens._Just killThread
