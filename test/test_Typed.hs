import Control.Arrow (second)
import Control.Monad.Identity (runIdentity)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Store.Guid as Guid
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import qualified Test.Framework as TestFramework

type Entity = Typed.ExpressionEntity ()

mkEntity :: Data.PureGuidExpression -> Entity
mkEntity =
  Typed.ExpressionEntity () . fmap mkEntity . Data.unPureGuidExpression

errorLoader :: Typed.Loader m
errorLoader = Typed.Loader $ error . show

withoutLoader ::
  Typed.ExpressionEntity s ->
  (Typed.Expression s, Typed.RefMap)
withoutLoader = runIdentity . Typed.inferFromEntity errorLoader Nothing

toExpression ::
  Data.PureGuidExpression -> (Typed.Expression (), Typed.RefMap)
toExpression = withoutLoader . mkEntity

showExpressionWithInferred :: Typed.RefMap -> Typed.Expression () -> String
showExpressionWithInferred refMap =
  List.intercalate "\n" . go
  where
    showDeref x =
      case Typed.deref refMap x of
      ([], pureExpr) -> show pureExpr
      other -> "ERROR: " ++ show other
    go typedExpr =
      [ "Expr: " ++ show (fmap (const ()) expr)
      , "  IVal:  " ++ showDeref val
      , "  IType: " ++ showDeref typ
      ] ++
      (map ("  " ++) . Foldable.concat .
       fmap go) expr
      where
        expr = Typed.eValue typedExpr
        Typed.TypedValue val typ = Typed.eInferred typedExpr

mkExpr ::
  String -> Data.Expression Data.PureGuidExpression ->
  Data.PureGuidExpression
mkExpr = Data.pureGuidExpression . Guid.fromString

hole :: Data.PureGuidExpression
hole = mkExpr "hole" Data.ExpressionHole

setType :: Data.PureGuidExpression
setType = mkExpr "set" Data.ExpressionSet

intType :: Data.PureGuidExpression
intType =
  mkExpr "int" . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Integer") setType

boolType :: Data.PureGuidExpression
boolType =
  mkExpr "bool" . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Bool") setType

zipMatch :: (a -> b -> Bool) -> [a] -> [b] -> Bool
zipMatch _ [] [] = True
zipMatch f (x:xs) (y:ys) = f x y && zipMatch f xs ys
zipMatch _ _ _ = False

compareDeref ::
  ([Typed.Conflict], Data.PureGuidExpression) ->
  ([Typed.Conflict], Data.PureGuidExpression) ->
  Bool
compareDeref (acs, aexpr) (bcs, bexpr) =
  zipMatch Typed.alphaEq acs bcs && Typed.alphaEq aexpr bexpr

removeBuiltinTypes :: Data.PureGuidExpression -> Data.PureGuidExpression
removeBuiltinTypes =
  Data.atPureGuidExpression $ Data.atGeValue f
  where
    f (Data.ExpressionBuiltin (Data.Builtin name _)) =
      Data.ExpressionBuiltin $ Data.Builtin name hole
    f x = fmap removeBuiltinTypes x

main :: IO ()
main = TestFramework.defaultMain
  [ testSuccessfulInferSame "literal int"
    (mkExpr "5" (Data.ExpressionLiteralInteger 5))
    [ intType ]
  , testSuccessfulInfer "simple application"
    (mkExpr "apply" (Data.makeApply hole hole))
    [ hole, hole, hole
    , mkExpr "" $ Data.makePi hole hole
    , hole, hole
    ]
  , testSuccessfulInferSame "application"
    (mkExpr "apply" (Data.makeApply funnyFunc hole))
    [ boolType
    , funnyFunc, funnyFuncType
    , funnyFuncType, setType
    , intType, setType, setType, setType
    , boolType, setType, setType, setType
    , hole, intType
    ]
  ]
  where
    funnyFunc =
      mkExpr "funnyFunc" . Data.ExpressionBuiltin $
      Data.Builtin (Data.FFIName ["Test"] "funnyFunc") funnyFuncType
    funnyFuncType = mkExpr "funcT" $ Data.makePi intType boolType
    testSuccessfulInferSame name pureExpr =
      testSuccessfulInfer name pureExpr . (pureExpr :)
    testSuccessfulInfer name pureExpr =
      testInfer name pureExpr . map ((,) [])
    testInfer name pureExpr result =
      testCase name .
      assertBool
        (unlines
         [ "Unexpected result expr:"
         , showExpressionWithInferred refMap typedExpr
         , show d
         , show result
         ]) . zipMatch compareDeref d $ (map . second) removeBuiltinTypes result
      where
        d = derefAll typedExpr
        (typedExpr, refMap) = toExpression pureExpr
        derefAll = map (Typed.deref refMap) . allRefs
    allRefs typedExpr =
      ([val, typ] ++) . Foldable.concat . fmap allRefs $
      Typed.eValue typedExpr
      where
        Typed.TypedValue val typ = Typed.eInferred typedExpr
