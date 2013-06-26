{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror #-}
module InferAssert where

import AnnotatedExpr
import Control.Applicative ((<$>), Applicative(..))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (runStateT, runState)
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression.IRef (DefI)
import System.IO (hPutStrLn, stderr)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import Utils
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified System.Random as Random
import qualified Test.Framework as TestFramework
import qualified Test.HUnit as HUnit

canonizeInferred :: InferResults t -> InferResults t
canonizeInferred =
  ExprUtil.randomizeParamIdsG (const ()) ExprUtil.debugNameGen Map.empty canonizePayload
  where
    canonizePayload gen guidMap (ival, ityp) =
      ( ExprUtil.randomizeParamIdsG (const ()) gen1 guidMap (\_ _ x -> x) ival
      , ExprUtil.randomizeParamIdsG (const ()) gen2 guidMap (\_ _ x -> x) ityp
      )
      where
        (gen1, gen2) = ExprUtil.ngSplit gen

assertCompareInferred ::
  InferResults t -> InferResults t -> HUnit.Assertion
assertCompareInferred result expected =
  assertBool errorMsg (null resultErrs)
  where
    resultC = canonizeInferred result
    expectedC = canonizeInferred expected
    (resultErrs, errorMsg) =
      errorMessage $ ExprUtil.matchExpression match mismatch resultC expectedC
    check s x y
      | ExprUtil.alphaEq x y = pure []
      | otherwise = fmap (: []) . addAnnotation $
        List.intercalate "\n"
        [ "  expected " ++ s ++ ":" ++ (show . simplifyDef) y
        , "  result   " ++ s ++ ":" ++ (show . simplifyDef) x
        ]
    match (v0, t0) (v1, t1) =
      (++) <$> check " type" t0 t1 <*> check "value" v0 v1
    mismatch e0 e1 =
      error $ concat
      [ "Result must have same expression shape:"
      , "\n Result:       ", redShow e0
      , "\n vs. Expected: ", redShow e1
      , "\n whole result:   ", redShow resultC
      , "\n whole expected: ", redShow expectedC
      ]
    redShow = ansiAround ansiRed . show . void

inferAssertion :: InferResults t -> HUnit.Assertion
inferAssertion expr =
  assertCompareInferred inferredExpr expr
  where
    inferredExpr = inferResults . fst . doInfer_ $ void expr

inferWVAssertion :: InferResults t -> InferResults t -> HUnit.Assertion
inferWVAssertion expr wvExpr = do
  -- TODO: assertCompareInferred should take an error prefix string,
  -- and do ALL the error printing itself. It has more information
  -- about what kind of error string would be useful.
  assertCompareInferred (inferResults inferredExpr) expr
    `E.onException` printOrig
  assertCompareInferred (inferResults wvInferredExpr) wvExpr
    `E.onException` (printOrig >> printWV)
  where
    printOrig = hPutStrLn stderr $ "WithoutVars:\n" ++ showInferredValType inferredExpr
    printWV = hPutStrLn stderr $ "WithVars:\n" ++ showInferredValType wvInferredExpr
    (inferredExpr, inferContext) = doInfer_ $ void expr
    wvInferredExpr = fst <$> wvInferredExprPL
    (wvInferredExprPL, _) =
      either error id $
      (`runStateT` inferContext)
      (ImplicitVariables.add (Random.mkStdGen 0)
       loader (flip (,) () <$> inferredExpr))

allowFailAssertion :: HUnit.Assertion -> HUnit.Assertion
allowFailAssertion assertion =
  (assertion >> successOccurred) `E.catch`
  \(E.SomeException _) -> errorOccurred
  where
    successOccurred =
      hPutStrLn stderr . ansiAround ansiYellow $ "NOTE: doesn't fail. Remove AllowFail?"
    errorOccurred =
      hPutStrLn stderr . ansiAround ansiYellow $ "WARNING: Allowing failure in:"

testInfer :: String -> InferResults t -> TestFramework.Test
testInfer name = testCase name . inferAssertion

testInferAllowFail :: String -> InferResults t -> TestFramework.Test
testInferAllowFail name expr =
  testCase name . allowFailAssertion $ inferAssertion expr

type InferredExpr t = ExprIRef.Expression t (Infer.Inferred (DefI t))

testResume ::
  String ->
  InferResults t ->
  Lens.Traversal' (InferredExpr t) (InferredExpr t) ->
  InferResults t ->
  TestFramework.Test
testResume name origExpr position newExpr =
  testCase name $ assertResume origExpr position newExpr

assertResume ::
  InferResults t ->
  Lens.Traversal' (InferredExpr t) (InferredExpr t) ->
  InferResults t ->
  HUnit.Assertion
assertResume origExpr position newExpr =
  void . E.evaluate . DeepSeq.force . (`runState` inferContext) $
  doInferM point newExpr
  where
    (tExpr, inferContext) = doInfer_ origExpr
    Just point = tExpr ^? position . Expr.ePayload . Lens.to Infer.iNode
