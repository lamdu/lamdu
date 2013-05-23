{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror #-}
module InferAssert where

import Control.Applicative ((<$>), Applicative(..))
import Control.Exception (evaluate)
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (runStateT, runState)
import InferCombinators
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..))
import Lamdu.Data.Expression.IRef (DefI)
import Test.HUnit (assertBool)
import Utils
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Test.HUnit as HUnit

assertCompareInferred ::
  InferResults t -> InferResults t -> HUnit.Assertion
assertCompareInferred result expected =
  assertBool errMsg (Map.null resultErrs)
  where
    errMsg =
      List.intercalate "\n" $ show
      (resultExpr
       & ExprLens.exprDef %~ defIStr
       & Lens.mapped %~ ixsStr) :
      "Errors:" :
      (map showErrItem . Map.toList) resultErrs
    showErrItem (ix, err) = ansiRedAround ("{" ++ show ix ++ "}:\n") ++ err
    defIStr = BS8.unpack . BS8.takeWhile (/= '\0') . Guid.bs . IRef.guid
    ixsStr [] = UnescapedStr ""
    ixsStr ixs = UnescapedStr . ansiRedAround . List.intercalate ", " $ map show ixs
    (resultExpr, resultErrs) =
      runState (ExprUtil.matchExpression match mismatch result expected) Map.empty
    addError msg = do
      ix <- State.gets Map.size
      State.modify $ Map.insert ix msg
      pure ix
    check s x y
      | ExprUtil.alphaEq x y = pure []
      | otherwise = fmap (: []) . addError $
        List.intercalate "\n"
        [ "  expected " ++ s ++ ":" ++ show y
        , "  result   " ++ s ++ ":" ++ show x
        ]
    match (v0, t0) (v1, t1) =
      (++) <$> check " type" t0 t1 <*> check "value" v0 v1
    mismatch _ _ = error "Result must have same expression shape!"

testInfer ::
  String -> InferResults t -> HUnit.Test
testInfer name expr =
  testCase name $ assertCompareInferred inferredExpr expr
  where
    inferredExpr = inferResults . fst . doInfer_ $ void expr

testCase :: String -> HUnit.Assertion -> HUnit.Test
testCase name = HUnit.TestLabel name . HUnit.TestCase

testResume ::
  String -> PureExprDefI t ->
  PureExprDefI t ->
  Lens.Traversal'
    (Expression (DefI t) (Infer.Inferred (DefI t)))
    (Expression (DefI t) (Infer.Inferred (DefI t))) ->
  HUnit.Test
testResume name newExpr testExpr lens =
  testCase name $
  let
    (tExpr, inferContext) = doInfer_ testExpr
    Just pl = tExpr ^? lens . Expr.ePayload
  in
    void . evaluate . (`runStateT` inferContext) $
    doInferM (Infer.iPoint pl) newExpr
