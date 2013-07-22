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
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Monoid (Monoid(..))
import InferCombinators
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import System.IO (hPutStrLn, stderr)
import Test.Framework (plusTestOptions)
import Test.Framework.Options (TestOptions'(..))
import Utils
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer as Infer
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

assertInferredEquals ::
  ExprInferred -> ExprInferred -> a -> a
assertInferredEquals result expected res
  | null resultErrs = res
  | otherwise = error errorMsg
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

verifyInferResult ::
  Expr.Expression (InferLoad.LoadedDef Def)
  (Infer.ScopedTypedValue, InputPayload) -> M ()
verifyInferResult exprLInferred = do
  exprDerefed <- deref (fst <$> exprLInferred)
  let exprInferred = exprLInferred & ExprLens.exprDef %~ (^. InferLoad.ldDef)
  return ()
    & assertInferredEquals exprDerefed (exprInferred <&> inferredOfInput . snd)

inferAssertion :: InputExpr -> HUnit.Assertion
inferAssertion origExpr =
  runContextAssertion $
  go =<< inferDef (infer =<< load origExpr)
  where
    go :: Expr.Expression (InferLoad.LoadedDef Def) (Infer.ScopedTypedValue, InputPayload) -> M ()
    go exprLInferred = do
      verifyInferResult exprLInferred
      (exprNextInput, flags) <- runWriterT $ handleResumption exprLInferred
      case flags of
        (Monoid.Any False, Monoid.Sum 0) -> return ()
        (Monoid.Any True, Monoid.Sum 0) -> error "NewInferred specified, but no subexpr has ResumeWith/ResumeOnSide"
        (_, _) -> go exprNextInput
    handleResumption ::
      Expr.Expression (InferLoad.LoadedDef Def) (Infer.ScopedTypedValue, InputPayload) ->
      WriterT (Monoid.Any, Monoid.Sum Int) M
      (Expr.Expression (InferLoad.LoadedDef Def) (Infer.ScopedTypedValue, InputPayload))
    handleResumption (Expr.Expression _ (stv, (InputPayload _ _ (ResumeWith newExpr)))) = do
      Writer.tell (Monoid.Any True, Monoid.Sum 1)
      lift $ loadInferInto stv newExpr
    handleResumption (Expr.Expression body (stv, (InputPayload _ _ (ResumeOnSide newExpr ipl)))) = do
      Writer.tell (Monoid.Any True, Monoid.Sum 1)
      lift $ verifyInferResult =<< loadInferInContext stv newExpr
      recurseBody body (stv, ipl)
    handleResumption (Expr.Expression body (stv, InputPayload _ _ (NewInferred ipl))) = do
      Writer.tell (Monoid.Any True, Monoid.Sum 0)
      recurseBody body (stv, ipl)
    handleResumption (Expr.Expression body (stv, pl@(InputPayload _ _ Same))) =
      recurseBody body (stv, pl)
    recurseBody body pl =
      (`Expr.Expression` pl) <$> Lens.traverse handleResumption body

expectLeft ::
  String -> (l -> HUnit.Assertion) ->
  Either l ExprInferred -> HUnit.Assertion
expectLeft _ handleLeft (Left x) = handleLeft x
expectLeft msg _ (Right x) =
  error $ unwords
  [ "Error", msg, "expected.  Unexpected success encountered:"
  , "\n", x & UnescapedStr . annotateTypes & show
  ]

inferFailsAssertion :: String -> (Error -> Bool) -> InputExpr -> HUnit.Assertion
inferFailsAssertion errorName isExpectedError expr =
  expectLeft errorName verifyError $
  runLoadInferDerefDef (void expr)
  where
    verifyError err
      | isExpectedError err = return ()
      | otherwise = error $ errorName ++ " error expected, but got: " ++ show err

-- inferWVAssertion :: ExprInferred -> ExprInferred -> HUnit.Assertion
-- inferWVAssertion expr wvExpr = do
--   -- TODO: assertInferredEquals should take an error prefix string,
--   -- and do ALL the error printing itself. It has more information
--   -- about what kind of error string would be useful.
--   assertInferredEquals (inferResults inferredExpr) expr
--     `E.onException` printOrig
--   assertInferredEquals (inferResults wvInferredExpr) wvExpr
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

testInfer :: String -> InputExpr -> TestFramework.Test
testInfer name = testCase name . inferAssertion

testInferAllowFail :: String -> String -> InputExpr -> TestFramework.Test
testInferAllowFail msg name expr =
  testCase name . allowFailAssertion msg $ inferAssertion expr

type ExprPosition def =
  forall a.
  Lens.Traversal'
  (Expr.Expression def a)
  (Expr.Expression def a)
