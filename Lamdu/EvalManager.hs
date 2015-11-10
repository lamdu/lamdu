{-# LANGUAGE NoImplicitPrelude, RankNTypes, LambdaCase #-}
module Lamdu.EvalManager
    ( Evaluators
    , new
    , start, stop
    , getResults
    , runTransactionAndMaybeRestartEvaluators
    ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.Utils (runAfter)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (join)
import           Data.CurAndPrev (CurAndPrev(..), prev, current)
import           Data.Foldable (traverse_)
import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import qualified Data.Map as Map
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
import           Lamdu.Eval.Results (EvalResults, erExprValues)
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

data Evaluators = Evaluators
    { eInvalidateCache :: IO ()
    , eDb :: Db
    , eEvaluatorRef :: IORef BGEvaluator
      -- TODO: Only store the prev here
    , eResultsRef :: IORef (CurAndPrev (EvalResults (ValI ViewM)))
    , eCancelTimerRef :: IORef (Maybe ThreadId)
    }

new :: IO () -> Db -> IO Evaluators
new invalidateCache db =
    do
        ref <- newIORef NotStarted
        resultsRef <- newIORef mempty
        cancelRef <- newIORef Nothing
        return Evaluators
            { eInvalidateCache = invalidateCache
            , eDb = db
            , eEvaluatorRef = ref
            , eResultsRef = resultsRef
            , eCancelTimerRef = cancelRef
            }

runViewTransactionInIO :: Db -> Transaction ViewM a -> IO a
runViewTransactionInIO db = DbLayout.runDbTransaction db . VersionControl.runAction

getResults :: Evaluators -> IO (CurAndPrev (EvalResults (ValI ViewM)))
getResults evaluators =
    do
        res <-
            readIORef (eEvaluatorRef evaluators) <&> startedEvaluator
            >>= maybe (return mempty) EvalBG.getResults
        atomicModifyIORef (eResultsRef evaluators)
            (join (,) . (current .~ res))

loadDef ::
    Evaluators -> DefI ViewM ->
    IO (Def.Definition (V.Val (ExprIRef.ValIProperty ViewM)) (DefI ViewM))
loadDef evaluators = runViewTransactionInIO (eDb evaluators) . Load.loadDef

evalActions :: Evaluators -> EvalBG.Actions (ValI ViewM)
evalActions evaluators =
    EvalBG.Actions
    { EvalBG._aLoadGlobal = loadGlobal
    , EvalBG._aRunBuiltin = Builtins.eval
    , EvalBG._aReportUpdatesAvailable = eInvalidateCache evaluators
    , EvalBG._aCompleted = \_ ->
      readIORef (eEvaluatorRef evaluators)
      <&> startedEvaluator
      >>= Lens._Just %%~ EvalBG.getStatus
      <&> Lens.has (Lens._Just . EvalBG._Finished)
      >>= \case
      False -> return ()
      True ->
          do
              atomicModifyIORef_ (eResultsRef evaluators) (prev .~ mempty)
              eInvalidateCache evaluators
    }
    where
        loadGlobal globalId =
            ExprIRef.defI globalId
            & loadDef evaluators
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

start :: Evaluators -> IO ()
start evaluators =
    Transaction.readIRef replIRef >>= ExprIRef.readVal
    & runViewTransactionInIO (eDb evaluators)
    >>= EvalBG.start (evalActions evaluators) <&> Started
    >>= writeIORef (eEvaluatorRef evaluators)

stop :: Evaluators -> IO ()
stop evaluators =
    readIORef (eEvaluatorRef evaluators)
    <&> startedEvaluator
    >>= traverse_ EvalBG.stop

sumDependency :: Set (ExprIRef.ValI ViewM) -> Set V.GlobalId -> Set Guid
sumDependency subexprs globals =
    mconcat
    [ Set.map (IRef.guid . ExprIRef.unValI) subexprs
    , Set.map (IRef.guid . ExprIRef.defI) globals
    ]

runTransactionAndMaybeRestartEvaluators :: Evaluators -> Transaction DbM a -> IO a
runTransactionAndMaybeRestartEvaluators evaluators transaction =
    readIORef (eEvaluatorRef evaluators)
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
                    stop evaluators
                    setCancelTimer evaluators
                    atomicModifyIORef_ (eResultsRef evaluators) pickPrevResults
                    start evaluators
                else EvalBG.resumeLoading eval
            return result
    where
        runTrans = DbLayout.runDbTransaction (eDb evaluators)

setCancelTimer :: Evaluators -> IO ()
setCancelTimer evaluators =
    do
        newCancelTimer <- runAfter 5000000 -- 5 seconds
            (atomicModifyIORef (eResultsRef evaluators)
                (flip (,) () . (prev .~ mempty))
            )
        atomicModifyIORef (eCancelTimerRef evaluators)
            (\x -> (Just newCancelTimer, x))
            >>= Lens.traverseOf_ Lens._Just killThread

pickPrevResults ::
    Ord pl => CurAndPrev (EvalResults pl) -> CurAndPrev (EvalResults pl)
pickPrevResults (CurAndPrev latest pre) =
    CurAndPrev { _current = mempty, _prev = newPrev }
    where
        newPrev
            | Map.size (latest ^. erExprValues) >
                Map.size (pre ^. erExprValues) = latest
            | otherwise = pre
