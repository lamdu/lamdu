{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror #-}
module InferAssert where

-- import Control.Monad.Trans.State (runStateT, runState)
-- import qualified Lamdu.Data.Infer.ImplicitVariables as ImplicitVariables
-- import qualified System.Random as Random
import AnnotatedExpr
import Control.Applicative ((<$>), Applicative(..))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Monoid (Monoid(..))
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import System.IO (hPutStrLn, stderr)
import Test.Framework (plusTestOptions)
import Test.Framework.Options (TestOptions'(..))
import Test.HUnit (assertBool)
import Utils
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.Load as InferLoad
import qualified Test.Framework as TestFramework
import qualified Test.Framework.Providers.HUnit as HUnitProvider
import qualified Test.HUnit as HUnit

canonizeInferred :: ExprInferred -> ExprInferred
canonizeInferred =
  ExprUtil.randomizeParamIdsG (const ()) ExprUtil.debugNameGen Map.empty canonizePayload
  where
    canonizePayload gen guidMap (ival, ityp) =
      ( ExprUtil.randomizeParamIdsG (const ()) gen1 guidMap (\_ _ x -> x) ival
      , ExprUtil.randomizeParamIdsG (const ()) gen2 guidMap (\_ _ x -> x) ityp
      )
      where
        (gen1, gen2) = ExprUtil.ngSplit gen

annotateTypes :: ExprInferred -> String
annotateTypes expr =
  expr
  & Lens.traverse %%~ annotate
  & errorMessage
  & snd
  where
    annotate (val, typ) =
      sequence
      [ addAnnotation ("Val:  " ++ show val)
      , addAnnotation ("Type: " ++ show typ)
      ]

assertCompareInferred ::
  ExprInferred -> ExprInferred -> HUnit.Assertion
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
        [ "  expected " ++ s ++ ":" ++ show y
        , "  result   " ++ s ++ ":" ++ show x
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

inferAssertion :: ExprInferred -> HUnit.Assertion
inferAssertion expr =
  assertCompareInferred inferredExpr expr
  where
    inferredExpr =
      assertSuccess . runNewContext . loadInferDef $ void expr

expectLeft ::
  String -> (l -> HUnit.Assertion) ->
  Either l ExprInferred -> HUnit.Assertion
expectLeft _ handleLeft (Left x) = handleLeft x
expectLeft msg _ (Right x) =
  error $ unwords
  [ "Error", msg, "expected.  Unexpected success encountered:"
  , "\n", x & UnescapedStr . annotateTypes & show
  ]

inferFailsAssertion :: String -> (Error -> Bool) -> ExprInferred -> HUnit.Assertion
inferFailsAssertion errorName isExpectedError expr =
  expectLeft errorName verifyError $
  runLoadInferDef (void expr)
  where
    verifyError err
      | isExpectedError err = return ()
      | otherwise = error $ errorName ++ " error expected, but got: " ++ show err

-- inferWVAssertion :: ExprInferred -> ExprInferred -> HUnit.Assertion
-- inferWVAssertion expr wvExpr = do
--   -- TODO: assertCompareInferred should take an error prefix string,
--   -- and do ALL the error printing itself. It has more information
--   -- about what kind of error string would be useful.
--   assertCompareInferred (inferResults inferredExpr) expr
--     `E.onException` printOrig
--   assertCompareInferred (inferResults wvInferredExpr) wvExpr
--     `E.onException` (printOrig >> printWV)
--   where
--     printOrig = hPutStrLn stderr $ "WithoutVars:\n" ++ showInferredValType inferredExpr
--     printWV = hPutStrLn stderr $ "WithVars:\n" ++ showInferredValType wvInferredExpr
--     (inferredExpr, inferContext) = doInfer_ $ void expr
--     wvInferredExpr = fst <$> wvInferredExprPL
--     (wvInferredExprPL, _) =
--       either error id $
--       (`runStateT` inferContext)
--       (ImplicitVariables.add (Random.mkStdGen 0)
--        loader (flip (,) () <$> inferredExpr))

allowFailAssertion :: String -> HUnit.Assertion -> HUnit.Assertion
allowFailAssertion msg assertion =
  (assertion >> successOccurred) `E.catch`
  \(E.SomeException _) -> errorOccurred
  where
    successOccurred =
      hPutStrLn stderr . ansiAround ansiYellow $ "NOTE: doesn't fail. Remove AllowFail?"
    errorOccurred =
      hPutStrLn stderr . ansiAround ansiYellow $
      concat ["WARNING: Allowing failure (", msg, ") in:"]

defaultTestOptions :: TestOptions' Maybe
defaultTestOptions = mempty { topt_timeout = Just (Just 100000) }

testCase :: TestFramework.TestName -> HUnit.Assertion -> TestFramework.Test
testCase name = plusTestOptions defaultTestOptions . HUnitProvider.testCase name

runContextAssertion :: M a -> HUnit.Assertion
runContextAssertion = void . E.evaluate . assertSuccess . runNewContext

testInfer :: String -> ExprInferred -> TestFramework.Test
testInfer name = testCase name . inferAssertion

testInferAllowFail :: String -> String -> ExprInferred -> TestFramework.Test
testInferAllowFail msg name expr =
  testCase name . allowFailAssertion msg $ inferAssertion expr

type ExprPosition def =
  forall a.
  Lens.Traversal'
  (Expr.Expression def a)
  (Expr.Expression def a)

testResume ::
  String -> ExprInferred ->
  ExprPosition (InferLoad.LoadedDef Def) ->
  ExprInferred ->
  TestFramework.Test
testResume name origExpr position newExpr =
  testCase name $ assertResume origExpr position newExpr

assertResume ::
  ExprInferred ->
  ExprPosition (InferLoad.LoadedDef Def) ->
  ExprInferred ->
  HUnit.Assertion
assertResume origExpr position newExpr =
  runContextAssertion $ do
    origInferred <- inferDef $ infer =<< load origExpr
    loadInferInto (origInferred ^?! position) newExpr
