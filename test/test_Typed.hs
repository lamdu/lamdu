{-# OPTIONS -Wall #-}
import Control.Monad (void)
import Control.Monad.Identity (runIdentity)
import qualified Data.Store.Guid as Guid
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import qualified Test.HUnit as HUnit

type Entity = Typed.ExpressionEntity String

mkEntity :: String -> Data.Expression Entity -> Entity
mkEntity s = Typed.ExpressionEntity s . Data.GuidExpression (Guid.fromString s)

errorLoader :: Typed.Loader m
errorLoader = Typed.Loader $ error . show

loadWithoutLoader ::
  Typed.ExpressionEntity s ->
  (Typed.Expression s, Typed.InferState)
loadWithoutLoader = runIdentity . Typed.fromLoaded errorLoader Nothing

loadExpression ::
  String -> Data.Expression Entity ->
  (Typed.Expression String, Typed.InferState, Typed.RefMap)
loadExpression s expr = (loaded, uninferredState, inferredState)
  where
    inferredState = Typed.infer uninferredState
    (loaded, uninferredState) = loadWithoutLoader $ mkEntity s expr

printInferExpression :: String -> Data.Expression Entity -> IO ()
printInferExpression s expr = do
  print loaded
  print inferredState
  where
    (loaded, _uninferredState, inferredState) = loadExpression s expr

hole :: Entity
hole = mkEntity "hole" Data.ExpressionHole

testLabel :: String -> HUnit.Assertion -> HUnit.Test
testLabel name = HUnit.TestLabel name . HUnit.TestCase

main :: IO ()
main = do
  -- void . HUnit.runTestTT $ HUnit.TestList
  -- [ testLabel "Test literal int" $
    printInferExpression "int5" $ Data.ExpressionLiteralInteger 5
  -- , testLabel "Test simple application" $
    printInferExpression "apply" $ Data.makeApply hole hole
  -- ]
