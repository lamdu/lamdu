{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror #-}
module InferAssert where

import Control.Applicative ((<$>), Applicative(..))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (runStateT, runState)
import InferCombinators
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..))
import Lamdu.Data.Expression.IRef (DefI)
import System.IO (hPutStrLn, stderr)
import Test.HUnit (assertBool)
import Utils
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified System.Random as Random
import qualified Test.HUnit as HUnit

simplifyDef :: Expression (DefI t) a -> Expression UnescapedStr a
simplifyDef =
  ExprLens.exprDef %~ defIStr
  where
    defIStr = UnescapedStr . BS8.unpack . BS8.takeWhile (/= '\0') . Guid.bs . IRef.guid

canonizeInferred ::
  Expression def0 (Expression def1 a, Expression def2 b) ->
  Expression def0 (Expression def1 a, Expression def2 b)
canonizeInferred expr =
  ExprUtil.randomizeParamIdsG ExprUtil.debugNameGen Map.empty canonizePayload expr
  where
    canonizePayload gen guidMap (ival, ityp) =
      ( ExprUtil.randomizeParamIdsG gen1 guidMap (\_ _ x -> x) ival
      , ExprUtil.randomizeParamIdsG gen2 guidMap (\_ _ x -> x) ityp
      )
      where
        (gen1, gen2) = ExprUtil.ngSplit gen

assertCompareInferred ::
  InferResults t -> InferResults t -> HUnit.Assertion
assertCompareInferred result expected =
  assertBool errMsg (Map.null resultErrs)
  where
    errMsg =
      List.intercalate "\n" $ (show . simplifyDef) (ixsStr <$> resultExpr) :
      "Errors:" :
      (map showErrItem . Map.toList) resultErrs
    showErrItem (ix, err) = ansiAround ansiRed ("{" ++ show ix ++ "}:\n") ++ err
    ixsStr [] = UnescapedStr ""
    ixsStr ixs = UnescapedStr . ansiAround ansiRed . List.intercalate ", " $ map show ixs
    resultC = canonizeInferred result
    expectedC = canonizeInferred expected
    (resultExpr, resultErrs) =
      runState (ExprUtil.matchExpression match mismatch resultC expectedC) Map.empty
    addError msg = do
      ix <- State.gets Map.size
      State.modify $ Map.insert ix msg
      pure ix
    check s x y
      | ExprUtil.alphaEq x y = pure []
      | otherwise = fmap (: []) . addError $
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

inferResults :: ExprIRef.Expression t (Infer.Inferred (DefI t)) -> InferResults t
inferResults = fmap (void . Infer.iValue &&& void . Infer.iType)

inferAssertion :: InferResults t -> HUnit.Assertion
inferAssertion expr =
  assertCompareInferred inferredExpr expr
  where
    inferredExpr = inferResults . fst . doInfer_ $ void expr

canonizeDebug :: Expression def a -> Expression def a
canonizeDebug = ExprUtil.randomizeParamIdsG ExprUtil.debugNameGen Map.empty (\_ _ -> id)

showInferred :: Expression (DefI t) Infer.IsRestrictedPoly -> String
showInferred =
  show . fmap restrictedStr . simplifyDef . canonizeDebug
  where
    restrictedStr Infer.UnrestrictedPoly = UnescapedStr ""
    restrictedStr Infer.RestrictedPoly = UnescapedStr $ ansiAround ansiYellow "R"

showInferredValType :: Expression def (Infer.Inferred (DefI t)) -> String
showInferredValType expr =
  unlines
  [ "Inferred val:  " ++ f Infer.iValue
  , "Inferred type: " ++ f Infer.iType
  ]
  where
    f t = expr ^. Expr.ePayload . Lens.to (showInferred . t)


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
      (ImplicitVariables.addVariables (Random.mkStdGen 0)
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

testInfer :: String -> InferResults t -> HUnit.Test
testInfer name = testCase name . inferAssertion

testInferAllowFail :: String -> InferResults t -> HUnit.Test
testInferAllowFail name expr =
  testCase name . allowFailAssertion $ inferAssertion expr

testCase :: String -> HUnit.Assertion -> HUnit.Test
testCase name = HUnit.TestLabel name . HUnit.TestCase

type InferredExpr t = Expression (DefI t) (Infer.Inferred (DefI t))
testResume ::
  String ->
  InferResults t ->
  Lens.Traversal' (InferredExpr t) (InferredExpr t) ->
  InferResults t ->
  HUnit.Test
testResume name origExpr position newExpr =
  testCase name $
  let
    (tExpr, inferContext) = doInfer_ origExpr
    Just point = tExpr ^? position . Expr.ePayload . Lens.to Infer.iPoint
  in
    void . E.evaluate . (`runStateT` inferContext) $
    doInferM point newExpr
