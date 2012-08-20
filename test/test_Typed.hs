{-# OPTIONS -Wall #-}
-- import Control.Monad (void)
import Control.Monad.Identity (runIdentity)
import qualified Data.Foldable as Foldable
import qualified Data.Store.Guid as Guid
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
-- import qualified Test.HUnit as HUnit

type Entity = Typed.ExpressionEntity ()

mkEntity :: String -> Data.Expression Entity -> Entity
mkEntity s = Typed.ExpressionEntity () . Data.GuidExpression (Guid.fromString s)

errorLoader :: Typed.Loader m
errorLoader = Typed.Loader $ error . show

withoutLoader ::
  Typed.ExpressionEntity s ->
  (Typed.Expression s, Typed.RefMap)
withoutLoader = runIdentity . Typed.inferFromEntity errorLoader Nothing

toExpression ::
  String -> Data.Expression Entity ->
  (Typed.Expression (), Typed.RefMap)
toExpression s = withoutLoader . mkEntity s

showExpressionWithInferred ::
  Typed.RefMap -> Typed.Expression () -> [String]
showExpressionWithInferred refMap typedExpr =
  [ "Expr: " ++ show (fmap (const ()) expr)
  , "  IVal:  " ++ showDeref val
  , "  IType: " ++ showDeref typ
  ] ++
  (map ("  " ++) . Foldable.concat .
   fmap (showExpressionWithInferred refMap)) expr
  where
    expr = Typed.eValue typedExpr
    showDeref x =
      case Typed.deref refMap x of
      ([], pureExpr) -> show pureExpr
      other -> "ERROR: " ++ show other
    Typed.TypedValue val typ = Typed.eInferred typedExpr

printInferExpression :: String -> Data.Expression Entity -> IO ()
printInferExpression s exprEntity = do
  putStrLn . unlines $ showExpressionWithInferred inferred typedExpr
  where
    (typedExpr, inferred) = toExpression s exprEntity

hole :: Entity
hole = mkEntity "hole" Data.ExpressionHole

-- testLabel :: String -> HUnit.Assertion -> HUnit.Test
-- testLabel name = HUnit.TestLabel name . HUnit.TestCase

main :: IO ()
main = do
  -- void . HUnit.runTestTT $ HUnit.TestList
  -- [ testLabel "Test literal int" $
    printInferExpression "int5" $ Data.ExpressionLiteralInteger 5
  -- , testLabel "Test simple application" $
    printInferExpression "apply" $ Data.makeApply hole hole
  -- ]
