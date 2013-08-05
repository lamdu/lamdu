{-# LANGUAGE RankNTypes #-}
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

assertInferredEquals :: String -> ExprInferred -> ExprInferred -> a -> a
assertInferredEquals errorPrefixStr result expected res
  | null resultErrs = res
  | otherwise = error $ errorPrefixStr ++ errorMsg
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
  String ->
  Expr.Expression (InferLoad.LoadedDef Def)
  (Infer.ScopedTypedValue Def, InputPayload) -> M ()
verifyInferResult msg exprLInferred = do
  exprDerefed <- deref (fst <$> exprLInferred)
  let exprInferred = exprLInferred & ExprLens.exprDef %~ (^. InferLoad.ldDef)
  return ()
    & assertInferredEquals msg exprDerefed (exprInferred <&> inferredOfInput . snd)

inferAssertion :: InputExpr -> HUnit.Assertion
inferAssertion origExpr =
  runContextAssertion $
  go (0 :: Int) =<< annotateErrors "initial infer: " (inferDef (infer =<< load origExpr))
  where
    msg count = "Resumption phase " ++ show count ++ " failed:\n"
    annotateErrors prefix action =
      either (error . (prefix ++) . show) return =<< try action
    go count exprLInferred = do
      verifyInferResult (msg count) exprLInferred
      (exprNextInput, (resumptionsExpected, resumptionsHappened)) <-
        annotateErrors (msg count) . runWriterT $
        handleResumption (verifyInferResult (msg (count+1))) exprLInferred
      case (resumptionsExpected, resumptionsHappened) of
        (Monoid.Any False, Monoid.Any False) -> return ()
        (Monoid.Any True, Monoid.Any False) ->
          error "NewInferred specified, but no subexpr has ResumeWith/ResumeOnSide"
        (_, _) -> go (count+1) exprNextInput
    handleResumption _ (Expr.Expression _ (stv, InputPayload _ _ (ResumeWith newExpr))) = do
      Writer.tell (Monoid.Any True, Monoid.Any True)
      lift $ loadInferInto stv newExpr
    handleResumption verify (Expr.Expression body (stv, InputPayload _ _ (ResumeOnSide newExpr ipl))) = do
      Writer.tell (Monoid.Any True, Monoid.Any True)
      () <- lift $ verify =<< loadInferInContext stv newExpr
      recurseBody verify body (stv, ipl)
    handleResumption verify (Expr.Expression body (stv, InputPayload _ _ (NewInferred ipl))) = do
      Writer.tell (Monoid.Any True, Monoid.Any False)
      recurseBody verify body (stv, ipl)
    handleResumption verify (Expr.Expression body (stv, ipl@(InputPayload _ _ Same))) =
      recurseBody verify body (stv, ipl)
    recurseBody verify body pl =
      (`Expr.Expression` pl) <$> Lens.traverse (handleResumption verify) body

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
--   assertInferredEquals origStr (inferResults inferredExpr) expr
--   assertInferredEquals wvStr (inferResults wvInferredExpr) wvExpr
--   where
--     origStr = "WithoutVars:\n" ++ showInferredValType inferredExpr
--     wvStr = origStr ++ "\nWithVars:\n" ++ showInferredValType wvInferredExpr
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
defaultTestOptions = mempty { topt_timeout = Just (Just 300000) }

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
