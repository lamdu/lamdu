{-# LANGUAGE RankNTypes #-}
module InferAssert where

import AnnotatedExpr
import Control.Applicative ((<$>), Applicative(..))
import Control.Lens.Operators
import Control.Monad (void, when)
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
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil
import qualified Lamdu.Data.Infer.Load as InferLoad
import qualified System.Random as Random
import qualified Test.Framework as TestFramework
import qualified Test.Framework.Providers.HUnit as HUnitProvider
import qualified Test.HUnit as HUnit

canonizeInferred :: ExprInferred -> ExprInferred
canonizeInferred =
  ExprUtil.randomizeParamIdsG (const ()) ExprUtil.debugNameGen Map.empty canonizePayload
  where
    canonizePayload gen guidMap (ival, ityp) =
      ( randomizeInferred gen1 ival
      , randomizeInferred gen2 ityp
      )
      where
        randomizeInferred g =
          ExprUtil.randomizeParamIdsG (const ()) g guidMap (\_ _ x -> x)
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
      errorMessage $ ExprUtil.matchExpr match mismatch resultC expectedC
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

verifyInferResult :: String -> InferredLoadedExpr InputPayload -> M ()
verifyInferResult msg exprLInferred = do
  exprDerefed <- deref (fst <$> exprLInferred)
  let exprInferred = exprLInferred & ExprLens.exprDef %~ (^. InferLoad.ldDef)
  return ()
    & assertInferredEquals msg exprDerefed (exprInferred <&> inferredOfInput . snd)

runSuccessfulM :: M a -> a
runSuccessfulM = fromRight . runNewContext

runContextAssertion :: M a -> HUnit.Assertion
runContextAssertion = void . E.evaluate . runSuccessfulM

inferAssertion :: InputExpr -> HUnit.Assertion
inferAssertion = runContextAssertion . inferVerifyExpr

inferVerifyExpr :: InputExpr -> M (InferredLoadedExpr InputPayload)
inferVerifyExpr origExpr =
  go (0 :: Int) =<< annotateErrors "initial infer: " (loadInferDef origExpr)
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
        (Monoid.Any False, Monoid.Any False) -> return exprNextInput
        (Monoid.Any True, Monoid.Any False) ->
          error "NewInferred specified, but no subexpr has ResumeWith/ResumeOnSide"
        (_, _) -> go (count+1) exprNextInput

haveAnyResumptions :: InputExpr -> Bool
haveAnyResumptions =
  Lens.anyOf (Lens.traverse . ipResumption) (not . isSame)
  where
    isSame Same = True
    isSame _ = False

withImplicitAssertion ::
  String -> (InferredLoadedExpr InputPayload -> M (InferredLoadedExpr a)) ->
  InputExpr -> InputExpr -> HUnit.Assertion
withImplicitAssertion opName f expr expected = runContextAssertion $ do
  finalExpr <- inferVerifyExpr expr
  withImplicitsExpr <- f finalExpr
  withImplicitsDerefed <- deref (fst <$> withImplicitsExpr)
  when (haveAnyResumptions expected) $ error "Resumptions not supported"
  return ()
    & assertInferredEquals (opName ++ " result mismatch")
      withImplicitsDerefed (expected <&> inferredOfInput)

inferWVAssertion :: InputExpr -> InputExpr -> HUnit.Assertion
inferWVAssertion = withImplicitAssertion "AddVariables" (addImplicitVariables (Random.mkStdGen 0) recursiveDefI)

inferWSAssertion :: InputExpr -> InputExpr -> HUnit.Assertion
inferWSAssertion = withImplicitAssertion "AddStructure" addStructure

handleResumption ::
  (InferredLoadedExpr InputPayload -> M ()) ->
  InferredLoadedExpr InputPayload ->
  WriterT (Monoid.Any, Monoid.Any) M (InferredLoadedExpr InputPayload)
handleResumption verifyInfersOnSide =
  go
  where
    recurseBody body pl = (`Expr.Expr` pl) <$> Lens.traverse go body
    go expr =
      case expr of
      Expr.Expr _ (stv, InputPayload _ _ (ResumeWith newExpr)) -> do
        Writer.tell (Monoid.Any True, Monoid.Any True)
        lift $ loadInferInto stv newExpr
      Expr.Expr body (stv, InputPayload _ _ (ResumeOnSide newExpr ipl)) -> do
        Writer.tell (Monoid.Any True, Monoid.Any True)
        () <- lift $ verifyInfersOnSide =<< loadInferInContext stv newExpr
        recurseBody body (stv, ipl)
      Expr.Expr body (stv, InputPayload _ _ (NewInferred ipl)) -> do
        Writer.tell (Monoid.Any True, Monoid.Any False)
        recurseBody body (stv, ipl)
      Expr.Expr body (stv, ipl@(InputPayload _ _ Same)) ->
        recurseBody body (stv, ipl)

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

testInfer :: String -> InputExpr -> TestFramework.Test
testInfer name = testCase name . inferAssertion

testInferAllowFail :: String -> String -> InputExpr -> TestFramework.Test
testInferAllowFail msg name expr =
  testCase name . allowFailAssertion msg $ inferAssertion expr

type ExprPosition def =
  forall a.
  Lens.Traversal'
  (Expr.Expr def a)
  (Expr.Expr def a)
