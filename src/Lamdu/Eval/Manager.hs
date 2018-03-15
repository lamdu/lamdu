{-# LANGUAGE NoImplicitPrelude, LambdaCase, TemplateHaskell #-}
module Lamdu.Eval.Manager
    ( Evaluator
    , NewParams(..), new
    , start, stop, executeReplIOProcess
    , getResults
    , runTransactionAndMaybeRestartEvaluator
    ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (runAfter)
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import qualified Data.Monoid as Monoid
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.JS as Eval
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.VersionControl (getVersion)
import qualified Lamdu.VersionControl as VersionControl
import           Revision.Deltum.Db (DB)
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
    , dbMVar :: MVar (Maybe DB)
    , copyJSOutputPath :: Maybe FilePath
    }

data Evaluator = Evaluator
    { eParams :: NewParams
    , eEvaluatorRef :: IORef BGEvaluator
    , eResultsRef :: IORef (EvalResults (ValI ViewM))
    , eCancelTimerRef :: IORef (Maybe ThreadId)
    }

new :: NewParams -> IO Evaluator
new params =
    do
        ref <- newIORef NotStarted
        resultsRef <- newIORef EvalResults.empty
        cancelRef <- newIORef Nothing
        pure Evaluator
            { eParams = params
            , eEvaluatorRef = ref
            , eResultsRef = resultsRef
            , eCancelTimerRef = cancelRef
            }

withDb :: MVar (Maybe DB) -> (DB -> IO a) -> IO a
withDb mvar action =
    withMVar mvar $ \case
    Nothing -> error "Trying to use DB when it is already gone"
    Just db -> action db

runViewTransactionInIO :: MVar (Maybe DB) -> T ViewM a -> IO a
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
        prevResults <- readIORef (eResultsRef evaluator)
        pure CurAndPrev { _prev = prevResults, _current = res }

eDb :: Evaluator -> MVar (Maybe DB)
eDb = dbMVar . eParams

loadDef ::
    Evaluator -> DefI ViewM ->
    IO (Def.Definition (Val (ExprIRef.ValIProperty ViewM)) (DefI ViewM))
loadDef evaluator = runViewTransactionInIO (eDb evaluator) . Load.def

evalActions :: Evaluator -> Eval.Actions (ValI ViewM)
evalActions evaluator =
    Eval.Actions
    { Eval._aLoadGlobal = loadGlobal
    , Eval._aReportUpdatesAvailable = resultsUpdated (eParams evaluator)
    , Eval._aCompleted = \_ ->
          do
              atomicModifyIORef_ (eResultsRef evaluator) (const EvalResults.empty)
              resultsUpdated (eParams evaluator)
    , Eval._aCopyJSOutputPath = copyJSOutputPath (eParams evaluator)
    }
    where
        loadGlobal globalId =
            ExprIRef.defI globalId
            & loadDef evaluator
            <&> Def.defBody . Lens.mapped . Lens.mapped %~ Property.value
            <&> Lens.mapped .~ ()

replIRef :: IRef ViewM (Def.Expr (ValI ViewM))
replIRef = DbLayout.repl DbLayout.codeIRefs

startBG :: Eval.Actions (ValI m) -> Def.Expr (Val (ValI m)) -> IO (Eval.Evaluator (ValI m))
startBG =
    Eval.start
    (IRef.uuid . ExprIRef.unValI)
    (ExprIRef.ValI . IRef.unsafeFromUUID)

start :: Evaluator -> IO ()
start evaluator =
    Transaction.readIRef replIRef
    >>= traverse ExprIRef.readVal
    & runViewTransactionInIO (eDb evaluator)
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
        writeIORef (eResultsRef evaluator) EvalResults.empty

executeReplIOProcess :: Evaluator -> IO ()
executeReplIOProcess = onEvaluator Eval.executeReplIOProcess

sumDependency :: Eval.Dependencies (ValI m) -> Set UUID
sumDependency (Eval.Dependencies subexprs globals) =
    mconcat
    [ Set.map (IRef.uuid . ExprIRef.unValI) subexprs
    , Set.map (IRef.uuid . ExprIRef.defI) globals
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
                    sumDependency rawDependencies & Set.insert (IRef.uuid replIRef)
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
                    atomicModifyIORef_ (eResultsRef evaluator) (const prevResults)
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
                atomicModifyIORef (eResultsRef evaluator)
                    (flip (,) () . const EvalResults.empty)
                resultsUpdated (eParams evaluator)
        atomicModifyIORef (eCancelTimerRef evaluator)
            (\x -> (Just newCancelTimer, x))
            >>= Lens.traverseOf_ Lens._Just killThread
