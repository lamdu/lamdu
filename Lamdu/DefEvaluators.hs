{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.DefEvaluators
    ( Evaluators(..)
    , new
    , start, stop
    , getResults
    , runTransactionAndMaybeRestartEvaluators
    ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.Utils (runAfter)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join)
import           Data.CurAndPrev (CurAndPrev(..), current, prev)
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
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
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (DbM, ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.Background as EvalBG
import           Lamdu.Eval.Results (EvalResults, erExprValues)
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.VersionControl as VersionControl

import           Prelude.Compat

data Evaluators = Evaluators
    { eInvalidateCache :: IO ()
    , eDb :: Db
    , eRef :: IORef (Map (DefI ViewM) (EvalBG.Evaluator (ValI ViewM)))
      -- TODO: Only store the prev here
    , eResultsRef :: IORef (CurAndPrev (EvalResults (ValI ViewM)))
    , eCancelTimerRef :: IORef (Maybe ThreadId)
    }

new :: IO () -> Db -> IO Evaluators
new invalidateCache db =
    do
        ref <- newIORef mempty
        resultsRef <- newIORef mempty
        cancelRef <- newIORef Nothing
        return Evaluators
            { eInvalidateCache = invalidateCache
            , eDb = db
            , eRef = ref
            , eResultsRef = resultsRef
            , eCancelTimerRef = cancelRef
            }

runViewTransactionInIO :: Db -> Transaction ViewM a -> IO a
runViewTransactionInIO db = DbLayout.runDbTransaction db . VersionControl.runAction

getResults :: Evaluators -> IO (CurAndPrev (EvalResults (ValI ViewM)))
getResults evaluators =
    do
        res <-
            readIORef (eRef evaluators)
            >>= mapM EvalBG.getResults . Map.elems
            <&> mconcat
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
    , EvalBG._aCompleted = \_ -> return ()
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

panesIRef :: IRef ViewM [Anchors.Pane ViewM]
panesIRef = DbLayout.panes DbLayout.codeIRefs

start :: Evaluators -> IO ()
start evaluators =
    Transaction.readIRef panesIRef
    & runViewTransactionInIO (eDb evaluators)
    >>= mapM tagLoadDef

    <&> mapMaybe (_2 %%~ (^? Def.defBody . Def._BodyExpr . Def.expr))
    <&> Lens.mapped . _2 . Lens.mapped %~ Property.value

    >>= Lens.traverse . _2 %%~ EvalBG.start (evalActions evaluators)
    <&> Map.fromList
    >>= writeIORef (eRef evaluators)
    where
        tagLoadDef defI = loadDef evaluators defI <&> (,) defI

stop :: Evaluators -> IO ()
stop evaluators =
    readIORef (eRef evaluators) >>= mapM_ EvalBG.stop . Map.elems

sumDependency ::
    ExprIRef.DefI ViewM -> (Set (ExprIRef.ValI ViewM), Set V.GlobalId) -> Set Guid
sumDependency defI (subexprs, globals) =
    mconcat
    [ Set.singleton (IRef.guid defI)
    , Set.map (IRef.guid . ExprIRef.unValI) subexprs
    , Set.map (IRef.guid . ExprIRef.defI) globals
    ]

runTransactionAndMaybeRestartEvaluators :: Evaluators -> Transaction DbM a -> IO a
runTransactionAndMaybeRestartEvaluators evaluators transaction =
    do
        defEvaluators <- readIORef (eRef evaluators)
        dependencies <-
            defEvaluators
            & Lens.traverse %%~ EvalBG.pauseLoading
            <&> Map.toList
            <&> Lens.mapped %~ uncurry sumDependency
            <&> mconcat
            <&> Set.insert (IRef.guid panesIRef)
        (dependencyChanged, result) <-
            do
                (oldVersion, result, newVersion) <-
                    (,,)
                    <$> VersionControl.getVersion
                    <*> transaction
                    <*> VersionControl.getVersion
                let checkDependencyChange versionData =
                        Version.changes versionData
                        <&> Change.objectKey
                        <&> (`Set.member` dependencies)
                        <&> Monoid.Any
                        & mconcat
                        & return
                Monoid.Any dependencyChanged <-
                    Version.walk checkDependencyChange checkDependencyChange oldVersion newVersion
                return (dependencyChanged, result)
            & DbLayout.runDbTransaction (eDb evaluators)
        if dependencyChanged
            then do
                stop evaluators
                setCancelTimer evaluators
                atomicModifyIORef (eResultsRef evaluators)
                    (flip (,) () . pickPrevResults)
                start evaluators
            else Map.elems defEvaluators & mapM_ EvalBG.resumeLoading
        return result

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
