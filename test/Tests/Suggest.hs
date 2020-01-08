{-# LANGUAGE TypeOperators #-}
-- | Test the suggest module

module Tests.Suggest where

import qualified Control.Lens as Lens
import           Control.Monad.State (runStateT)
import           Hyper
import           Hyper.Infer
import           Hyper.Type.AST.FuncType
import           Hyper.Type.AST.Row
import           Hyper.Unify.Binding
import           Hyper.Unify.New (newTerm, newUnbound)
import           Lamdu.Calc.Infer
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Convert.Hole.Suggest (termTransforms)
import           Revision.Deltum.Transaction (Transaction)
import           Test.HUnit (assertBool)

import           Test.Lamdu.Prelude

type T = Transaction

testApplyForms :: Test
testApplyForms =
    case res of
    [s] ->
        Lens.has (hVal . V._BApp . V.appArg . hVal . V._BRecExtend . eKey . Lens.only "a") s
        & assertBool "termTransforms did not create argument record"
    _ -> fail "termTransforms on func failed to suggest exactly 1 option"
    & testCase "apply-forms"
    where
        res =
            termTransforms V.emptyScope id id func
            & (`runStateT` state)
            <&> fst
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
test :: Test
test =
    testGroup "suggest-tests"
    [ testApplyForms
    ]
