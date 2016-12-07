{-# LANGUAGE NoImplicitPrelude, RankNTypes, LambdaCase #-}
module Lamdu.Eval.Manager
    ( Evaluator
    , NewParams(..), new
    , start, stop
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
import qualified Data.Set as Set
import           Data.Store.Db (Db)
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (DbM, ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.JS as Eval
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.VersionControl (getVersion)
import qualified Lamdu.VersionControl as VersionControl

import           Lamdu.Prelude

type T = Transaction

data BGEvaluator = NotStarted | Started (Eval.Evaluator (ValI ViewM))

startedEvaluator :: BGEvaluator -> Maybe (Eval.Evaluator (ValI ViewM))
startedEvaluator NotStarted = Nothing
startedEvaluator (Started eval) = Just eval

data NewParams = NewParams
    { invalidateCache :: IO ()
    , dbMVar :: MVar (Maybe Db)
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
        return Evaluator
            { eParams = params
            , eEvaluatorRef = ref
            , eResultsRef = resultsRef
            , eCancelTimerRef = cancelRef
            }

withDb :: MVar (Maybe Db) -> (Db -> IO a) -> IO a
withDb mvar action =
    withMVar mvar $ \case
    Nothing -> error "Trying to use DB when it is already gone"
    Just db -> action db

runViewTransactionInIO :: MVar (Maybe Db) -> T ViewM a -> IO a
runViewTransactionInIO dbM trans =
    withDb dbM $ \db ->
    DbLayout.runDbTransaction db (VersionControl.runAction trans)

getLatestResults :: Evaluator -> IO (EvalResults (ValI ViewM))
getLatestResults evaluator =
    readIORef (eEvaluatorRef evaluator) <&> startedEvaluator
    >>= maybe (return EvalResults.empty) Eval.getResults

getResults :: Evaluator -> IO (CurAndPrev (EvalResults (ValI ViewM)))
getResults evaluator =
    do
        res <- getLatestResults evaluator
        prevResults <- readIORef (eResultsRef evaluator)
        return CurAndPrev { _prev = prevResults, _current = res }

eDb :: Evaluator -> MVar (Maybe Db)
eDb = dbMVar . eParams

loadDef ::
    Evaluator -> DefI ViewM ->
    IO (Def.Definition (Val (ExprIRef.ValIProperty ViewM)) (DefI ViewM))
loadDef evaluator = runViewTransactionInIO (eDb evaluator) . Load.def

readAssocName :: Evaluator -> UUID -> IO Text
readAssocName evaluator uuid =
    Transaction.getP (Anchors.assocNameRef uuid)
    & runViewTransactionInIO (eDb evaluator)

evalActions :: Evaluator -> Eval.Actions (ValI ViewM)
evalActions evaluator =
    Eval.Actions
    { Eval._aLoadGlobal = loadGlobal
    , Eval._aReadAssocName = readAssocName evaluator
    , Eval._aReportUpdatesAvailable = invalidateCache (eParams evaluator)
    , Eval._aCompleted = \_ ->
          do
              atomicModifyIORef_ (eResultsRef evaluator) (const EvalResults.empty)
              invalidateCache (eParams evaluator)
    , Eval._aCopyJSOutputPath = copyJSOutputPath (eParams evaluator)
    }
    where
        loadGlobal globalId =
            ExprIRef.defI globalId
            & loadDef evaluator
            <&> asDef
        asDef x =
            x ^. Def.defBody
            <&> Lens.mapped %~ Property.value

replIRef :: IRef ViewM (ValI ViewM)
replIRef = DbLayout.repl DbLayout.codeIRefs

startBG :: Eval.Actions (ValI m) -> Val (ValI m) -> IO (Eval.Evaluator (ValI m))
startBG =
    Eval.start
    (IRef.uuid . ExprIRef.unValI)
    (ExprIRef.ValI . IRef.unsafeFromUUID)

start :: Evaluator -> IO ()
start evaluator =
    Transaction.readIRef replIRef >>= ExprIRef.readVal
    & runViewTransactionInIO (eDb evaluator)
    >>= startBG
        (evalActions evaluator) <&> Started
    >>= writeIORef (eEvaluatorRef evaluator)

stop :: Evaluator -> IO ()
stop evaluator =
    do
        readIORef (eEvaluatorRef evaluator)
            <&> startedEvaluator
            >>= traverse_ Eval.stop
        writeIORef (eEvaluatorRef evaluator) NotStarted
        writeIORef (eResultsRef evaluator) EvalResults.empty

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
                            <&> Monoid.Any & mconcat & return
                    Monoid.Any dependencyChanged <-
                        Version.walk checkDependencyChange checkDependencyChange oldVersion newVersion
                    return (dependencyChanged, result)
                & runTrans
            when dependencyChanged $
                do
                    prevResults <- getLatestResults evaluator
                    stop evaluator
                    setCancelTimer evaluator
                    atomicModifyIORef_ (eResultsRef evaluator) (const prevResults)
                    start evaluator
            return result
    where
        runTrans trans =
            withDb (eDb evaluator) $ \db -> DbLayout.runDbTransaction db trans

setCancelTimer :: Evaluator -> IO ()
setCancelTimer evaluator =
    do
        newCancelTimer <- runAfter 5000000 -- 5 seconds
            (atomicModifyIORef (eResultsRef evaluator)
                (flip (,) () . const EvalResults.empty)
            )
        atomicModifyIORef (eCancelTimerRef evaluator)
            (\x -> (Just newCancelTimer, x))
            >>= Lens.traverseOf_ Lens._Just killThread
