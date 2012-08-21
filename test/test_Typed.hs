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

errorLoader :: Typed.Loader m
errorLoader = Typed.Loader $ error . show

toExpression ::
  Data.PureExpression ->
  (Typed.Expression (), Typed.RefMap)
toExpression = runIdentity . Typed.inferFromEntity errorLoader Nothing

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
        expr = Data.eValue typedExpr
        Typed.TypedValue val typ = Typed.iRefs $ Data.ePayload typedExpr

mkExpr ::
  String -> Data.ExpressionBody Data.PureExpression ->
  Data.PureExpression
mkExpr = Data.pureExpression . Guid.fromString

hole :: Data.PureExpression
hole = mkExpr "hole" Data.ExpressionHole

setType :: Data.PureExpression
setType = mkExpr "set" Data.ExpressionSet

intType :: Data.PureExpression
intType =
  mkExpr "int" . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Integer") setType

boolType :: Data.PureExpression
boolType =
  mkExpr "bool" . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Bool") setType

zipMatch :: (a -> b -> Bool) -> [a] -> [b] -> Bool
zipMatch _ [] [] = True
zipMatch f (x:xs) (y:ys) = f x y && zipMatch f xs ys
zipMatch _ _ _ = False

compareDeref ::
  ([Typed.Conflict], Data.PureExpression) ->
  ([Typed.Conflict], Data.PureExpression) ->
  Bool
compareDeref (acs, aexpr) (bcs, bexpr) =
  zipMatch Typed.alphaEq acs bcs && Typed.alphaEq aexpr bexpr

removeBuiltinTypes :: Data.PureExpression -> Data.PureExpression
removeBuiltinTypes =
  Data.atEValue f
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
    [ hole, hole
    , hole, mkExpr "" $ Data.makePi hole hole
    , hole, hole
    ]
  , testSuccessfulInferSame "application"
    (mkExpr "apply" (Data.makeApply funnyFunc hole))
    [ boolType
    , funnyFunc, funnyFuncType
    , funnyFuncType, setType, intType, setType, setType, setType, boolType, setType, setType, setType
    , hole, intType
    ]
  , testSuccessfulInfer "apply on var"
    (makeFunnyLambda "lambda"
      (mkExpr "applyInner" (Data.makeApply hole
        (mkExpr "var" (Data.ExpressionGetVariable (
          Data.ParameterRef (Guid.fromString "lambda"))
        ))
      ))
    )
    [ makeFunnyLambda "" hole
    , mkExpr "" $ Data.makePi hole boolType
    , hole, setType
    , mkExpr "" $ Data.makeApply funnyFunc hole, boolType
    , funnyFunc, funnyFuncType
    , funnyFuncType, setType, intType, setType, setType, setType, boolType, setType, setType, setType
    , hole, intType
    , hole, mkExpr "" $ Data.makePi hole intType
    , mkExpr "" . Data.ExpressionGetVariable . Data.ParameterRef $ Guid.fromString "lambda", hole
    ]
  ]
  where
    makeFunnyLambda g =
      mkExpr g .
      Data.makeLambda hole .
      mkExpr "applyFunny" .
      Data.makeApply funnyFunc
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
      Data.eValue typedExpr
      where
        Typed.TypedValue val typ = Typed.iRefs $ Data.ePayload typedExpr
