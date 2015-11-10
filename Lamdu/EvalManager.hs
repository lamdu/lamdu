{-# LANGUAGE NoImplicitPrelude, RankNTypes, LambdaCase #-}
module Lamdu.EvalManager
    ( Evaluator
    , new
    , start, stop
    , getResults
    , runTransactionAndMaybeRestartEvaluator
    ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.Utils (runAfter)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Foldable (traverse_)
import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import qualified Data.Monoid as Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Db (Db)
import           Data.Store.Guid (Guid)
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins as Builtins
import qualified Lamdu.Builtins.Anchors as BuiltinAnchors
import           Lamdu.Data.DbLayout (DbM, ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.Background as EvalBG
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Val as V
import           Lamdu.VersionControl (getVersion)
import qualified Lamdu.VersionControl as VersionControl

import           Prelude.Compat

data BGEvaluator = NotStarted | Started (EvalBG.Evaluator (ValI ViewM))

startedEvaluator :: BGEvaluator -> Maybe (EvalBG.Evaluator (ValI ViewM))
startedEvaluator NotStarted = Nothing
startedEvaluator (Started eval) = Just eval

data Evaluator = Evaluator
    { eInvalidateCache :: IO ()
    , eDb :: Db
    , eEvaluatorRef :: IORef BGEvaluator
    , eResultsRef :: IORef (EvalResults (ValI ViewM))
    , eCancelTimerRef :: IORef (Maybe ThreadId)
    }

new :: IO () -> Db -> IO Evaluator
new invalidateCache db =
    do
        ref <- newIORef NotStarted
        resultsRef <- newIORef mempty
        cancelRef <- newIORef Nothing
        return Evaluator
            { eInvalidateCache = invalidateCache
            , eDb = db
            , eEvaluatorRef = ref
            , eResultsRef = resultsRef
            , eCancelTimerRef = cancelRef
            }

runViewTransactionInIO :: Db -> Transaction ViewM a -> IO a
runViewTransactionInIO db = DbLayout.runDbTransaction db . VersionControl.runAction

getLatestResults :: Evaluator -> IO (EvalResults (ValI ViewM))
getLatestResults evaluator =
    readIORef (eEvaluatorRef evaluator) <&> startedEvaluator
    >>= maybe (return mempty) EvalBG.getResults

getResults :: Evaluator -> IO (CurAndPrev (EvalResults (ValI ViewM)))
getResults evaluator =
    do
        res <- getLatestResults evaluator
        prevResults <- readIORef (eResultsRef evaluator)
        return CurAndPrev { _prev = prevResults, _current = res }

loadDef ::
    Evaluator -> DefI ViewM ->
    IO (Def.Definition (V.Val (ExprIRef.ValIProperty ViewM)) (DefI ViewM))
loadDef evaluator = runViewTransactionInIO (eDb evaluator) . Load.loadDef

evalActions :: Evaluator -> EvalBG.Actions (ValI ViewM)
evalActions evaluator =
    EvalBG.Actions
    { EvalBG._aLoadGlobal = loadGlobal
    , EvalBG._aRunBuiltin = Builtins.eval
    , EvalBG._aReportUpdatesAvailable = eInvalidateCache evaluator
    , EvalBG._aCompleted = \_ ->
      readIORef (eEvaluatorRef evaluator)
      <&> startedEvaluator
      >>= Lens._Just %%~ EvalBG.getStatus
      <&> Lens.has (Lens._Just . EvalBG._Finished)
      >>= \case
      False -> return ()
      True ->
          do
              atomicModifyIORef_ (eResultsRef evaluator) (const mempty)
              eInvalidateCache evaluator
    }
    where
        loadGlobal globalId =
            ExprIRef.defI globalId
            & loadDef evaluator
            <&> asDef globalId
        asDef globalId x =
            x ^. Def.defBody
            <&> Lens.mapped %~ Property.value
            <&> replaceRecursiveReferences globalId
            & Just
        replaceRecursiveReferences globalId (V.Val pl (V.BLeaf (V.LVar v)))
            | v == BuiltinAnchors.recurseVar = V.Val pl (V.BLeaf (V.LGlobal globalId))
        replaceRecursiveReferences globalId val =
            val & V.body . Lens.traversed %~ replaceRecursiveReferences globalId

replIRef :: IRef ViewM (ValI ViewM)
replIRef = DbLayout.repl DbLayout.codeIRefs

start :: Evaluator -> IO ()
start evaluator =
    Transaction.readIRef replIRef >>= ExprIRef.readVal
    & runViewTransactionInIO (eDb evaluator)
    >>= EvalBG.start (evalActions evaluator) <&> Started
    >>= writeIORef (eEvaluatorRef evaluator)

stop :: Evaluator -> IO ()
stop evaluator =
    do
        readIORef (eEvaluatorRef evaluator)
            <&> startedEvaluator
            >>= traverse_ EvalBG.stop
        writeIORef (eEvaluatorRef evaluator) NotStarted
        writeIORef (eResultsRef evaluator) mempty

sumDependency :: Set (ExprIRef.ValI ViewM) -> Set V.GlobalId -> Set Guid
sumDependency subexprs globals =
    mconcat
    [ Set.map (IRef.guid . ExprIRef.unValI) subexprs
    , Set.map (IRef.guid . ExprIRef.defI) globals
    ]

runTransactionAndMaybeRestartEvaluator :: Evaluator -> Transaction DbM a -> IO a
runTransactionAndMaybeRestartEvaluator evaluator transaction =
    readIORef (eEvaluatorRef evaluator)
    >>= \case
    NotStarted -> runTrans transaction
    Started eval ->
        do
            dependencies <-
                EvalBG.pauseLoading eval
                <&> uncurry sumDependency
                <&> Set.insert (IRef.guid replIRef)
            (dependencyChanged, result) <-
                do
                    (oldVersion, result, newVersion) <-
                        (,,) <$> getVersion <*> transaction <*> getVersion
                    let checkDependencyChange versionData =
                            Version.changes versionData
                            <&> Change.objectKey <&> (`Set.member` dependencies)
                            <&> Monoid.Any & mconcat & return
                    Monoid.Any dependencyChanged <-
                        Version.walk checkDependencyChange checkDependencyChange oldVersion newVersion
                    return (dependencyChanged, result)
                & runTrans
            if dependencyChanged
                then do
                    prevResults <- getLatestResults evaluator
                    stop evaluator
                    setCancelTimer evaluator
                    atomicModifyIORef_ (eResultsRef evaluator) (const prevResults)
                    start evaluator
                else EvalBG.resumeLoading eval
            return result
    where
        runTrans = DbLayout.runDbTransaction (eDb evaluator)

setCancelTimer :: Evaluator -> IO ()
setCancelTimer evaluator =
    do
        newCancelTimer <- runAfter 5000000 -- 5 seconds
            (atomicModifyIORef (eResultsRef evaluator)
                (flip (,) () . const mempty)
            )
        atomicModifyIORef (eCancelTimerRef evaluator)
            (\x -> (Just newCancelTimer, x))
            >>= Lens.traverseOf_ Lens._Just killThread
