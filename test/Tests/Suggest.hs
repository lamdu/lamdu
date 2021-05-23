{-# LANGUAGE TypeApplications #-}
-- | Test the suggest module

module Tests.Suggest where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Control.Monad.State (runStateT)
import           Data.List.Class (toList)
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer
import           Hyper.Recurse
import           Hyper.Type.AST.FuncType
import           Hyper.Type.AST.Row
import           Hyper.Type.AST.Scheme
import           Hyper.Type.AST.Nominal
import           Hyper.Unify.Binding
import           Hyper.Unify.New (newTerm, newUnbound)
import           Lamdu.Calc.Definition (Deps(..))
import           Lamdu.Calc.Infer
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Db.Layout (ViewM, runDbTransaction)
import           Lamdu.VersionControl (runAction)
import           Lamdu.Sugar.Convert.Hole.Suggest (termTransforms, termTransformsWithModify)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

runDb :: Transaction ViewM a -> IO a
runDb a = withDB "data/freshdb.json" (`runDbTransaction` runAction a)

testApplyForms :: Test
testApplyForms =
    termTransforms @(ListT _) (pure "var") V.emptyScope id id func
    & (`runStateT` state) <&> fst & toList & runDb
    >>=
    \case
    [s] ->
        Lens.has (hVal . V._BApp . V.appArg . hVal . V._BRecExtend . eKey . Lens.only "a") s
        & assertBool "termTransforms did not create argument record"
    _ -> fail "termTransforms on func failed to suggest exactly 1 option"
    & testCase "apply-forms"
    where
        func = Ann (inferResult # funType) (V.BLeaf V.LHole)
        (funType, state) =
            either (error . show) id $
            runPureInfer V.emptyScope
            (InferState emptyPureInferState varGen) $
            do
                paramsRecord <-
                    V.RowExtend "a" <$> newUnbound <*> newTerm T.REmpty
                    <&> T.RExtend
                    >>= newTerm
                    <&> T.TRecord
                    >>= newTerm
                FuncType paramsRecord <$> newUnbound
                    <&> T.TFun
                    >>= newTerm :: PureInfer (V.Scope # UVar) (UVar # T.Type)

-- Tests that nullary inject in nominals are suggested
-- see https://trello.com/c/AKfdwwDK/494-nullary-injects-results-are-cumbersome
testBool :: Test
testBool =
    termTransformsWithModify @(ListT _) (pure "var") V.emptyScope id id expr
    & (`runStateT` state) <&> fst & toList & runDb
    >>=
    \case
    [s] ->
        Lens.has
        ( hVal . V._BToNom . tnVal . hVal . V._BApp
        . Lens.filteredBy (V.appFunc . hVal . V._BLeaf . V._LInject)
        . V.appArg . hVal . V._BLeaf . V._LRecEmpty
        ) s
        & assertBool "didn't complete nullary inject in nominal"
    _ -> fail "termTransformsWithModify didn't result with 1 option"
    & testCase "bool"
    where
        inferExample :: PureInfer (V.Scope # UVar) (Ann (InferResult UVar) # V.Term)
        inferExample =
            V.BToNomP "Bool" (V.BLeafP (V.LInject "True") `V.BAppP` V.BLeafP V.LHole) ^. hPlain
            & wrap (\_ x -> Ann (Const ()) x)
            & infer
            <&> hflipped %~ hmap (const (^. _2))
        (expr, state) =
            loadDeps deps >>= (`local` inferExample)
            & runPureInfer V.emptyScope (InferState emptyPureInferState varGen)
            & either (error . show) id

deps :: Deps
deps =
    Deps
    { _depsNominals = Map.fromList [boolTypePair]
    , _depsGlobalTypes = mempty
    }

boolTypePair :: (T.NominalId, Pure # NominalDecl T.Type)
boolTypePair =
    ( "Bool"
    , _Pure # NominalDecl
        { _nParams = T.Types mempty mempty
        , _nScheme =
            _Pure # T.REmpty
            & RowExtend "True" emptyRec & T.RExtend & Pure
            & RowExtend "False" emptyRec & T.RExtend & Pure
            & T.TVariant & Pure
            & Scheme (T.Types mempty mempty)
        }
    )
    where
        emptyRec = Pure T.REmpty & T.TRecord & Pure

test :: Test
test =
    testGroup "suggest-tests"
    [ testApplyForms
    , testBool
    ]
