import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Monad.Identity (runIdentity)
import Data.Maybe (isJust)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import qualified Test.Framework as TestFramework

errorLoader :: Typed.Loader m
errorLoader = Typed.Loader $ error . show

toExpression ::
  Data.PureExpression ->
  (Typed.Expression (), Typed.RefMap)
toExpression = runIdentity . Typed.inferFromEntity errorLoader Nothing

mkExpr ::
  String -> Data.ExpressionBody Data.PureExpression ->
  Data.PureExpression
mkExpr = Data.pureExpression . Guid.fromString

hole :: Data.PureExpression
hole = mkExpr "hole" $ Data.ExpressionLeaf Data.Hole

setType :: Data.PureExpression
setType = mkExpr "set" $ Data.ExpressionLeaf Data.Set

intType :: Data.PureExpression
intType =
  mkExpr "int" . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Integer") setType

boolType :: Data.PureExpression
boolType =
  mkExpr "bool" . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Bool") setType

removeBuiltinTypes :: Data.PureExpression -> Data.PureExpression
removeBuiltinTypes =
  Data.atEValue f
  where
    f (Data.ExpressionBuiltin (Data.Builtin name _)) =
      Data.ExpressionBuiltin $ Data.Builtin name hole
    f x = fmap removeBuiltinTypes x

type InferredExpr = ([Typed.Conflict], Data.PureExpression)
type InferResults = Data.Expression (InferredExpr, InferredExpr)

inferResults :: Typed.Expression s -> InferResults
inferResults =
  fmap f
  where
    f inferred = (Typed.iValue inferred, Typed.iType inferred)

showExpressionWithInferred :: InferResults -> String
showExpressionWithInferred =
  List.intercalate "\n" . go
  where
    showInferred ([], pureExpr) = show pureExpr
    showInferred x = "ERROR: " ++ show x
    go typedExpr =
      [ "Expr: " ++ show (fmap (const ()) expr)
      , "  IVal:  " ++ showInferred val
      , "  IType: " ++ showInferred typ
      ] ++
      (map ("  " ++) . Foldable.concat .
       fmap go) expr
      where
        expr = Data.eValue typedExpr
        (val, typ) = Data.ePayload typedExpr

compareInferred :: InferResults -> InferResults -> Bool
compareInferred x y =
  isJust $ Traversable.sequence =<< Data.matchExpression f x y
  where
    f (v0, t0) (v1, t1) =
      liftA2 (,) (matchI v0 v1) (matchI t0 t1)
    matchI (c0, r0) (c1, r1) = do
      guard $ length c0 == length c1
      liftA2 (,)
        (sequence (zipWith matchPure c0 c1))
        (matchPure r0 r1)
    matchPure = Data.matchExpression nop
    nop () () = ()

mkInferredLeaf :: Data.ExpressionBody () -> Data.PureExpression -> InferResults
mkInferredLeaf leaf typ =
  Data.Expression
  { Data.eGuid = Guid.fromString "leaf"
  , Data.eValue = fmap (error errMsg) leaf
  , Data.ePayload =
      ( ([], Data.pureExpression g (fmap (error errMsg) leaf))
      , ([], typ)
      )
  }
  where
    errMsg = "leaf shouldn't have children"
    g = Guid.fromString "leaf"

mkInferredNode ::
  String -> Data.PureExpression -> Data.PureExpression ->
  Data.ExpressionBody InferResults -> InferResults
mkInferredNode g iVal iType body =
  Data.Expression (Guid.fromString g) body (([], iVal), ([], iType))

main :: IO ()
main = TestFramework.defaultMain
  [ testInfer "literal int"
    (mkExpr "5" (Data.ExpressionLeaf (Data.LiteralInteger 5)))
    (mkInferredLeaf (Data.ExpressionLeaf (Data.LiteralInteger 5)) intType)
  , testInfer "simple apply"
    (mkExpr "apply" (Data.makeApply hole hole))
    (mkInferredNode "" hole hole
      (Data.makeApply
        (mkInferredLeaf (Data.ExpressionLeaf Data.Hole) (mkExpr "" (Data.makePi hole hole)))
        (mkInferredLeaf (Data.ExpressionLeaf Data.Hole) hole)
      )
    )
  , testInfer "apply"
    (mkExpr "apply" (Data.makeApply funnyFunc hole))
    (mkInferredNode ""
      (mkExpr "" (Data.makeApply (removeBuiltinTypes funnyFunc) hole))
      (removeBuiltinTypes boolType)
      (Data.makeApply
        (mkInferredNode ""
          (removeBuiltinTypes funnyFunc)
          (removeBuiltinTypes funnyFuncType)
          (Data.ExpressionBuiltin $ Data.Builtin funnyFuncName
            (mkInferredNode ""
              (removeBuiltinTypes funnyFuncType)
              setType
              (Data.makePi
                (mkInferredNode ""
                  (removeBuiltinTypes intType)
                  setType
                  (Data.ExpressionBuiltin . Data.Builtin (Data.FFIName ["Prelude"] "Integer") $
                    mkInferredLeaf (Data.ExpressionLeaf Data.Set) setType
                  )
                )
                (mkInferredNode ""
                  (removeBuiltinTypes boolType)
                  setType
                  (Data.ExpressionBuiltin . Data.Builtin (Data.FFIName ["Prelude"] "Bool") $
                    mkInferredLeaf (Data.ExpressionLeaf Data.Set) setType
        ) ) ) ) ) )
        (mkInferredLeaf (Data.ExpressionLeaf Data.Hole) (removeBuiltinTypes intType))
      )
    )
  --, testInfer "apply on var"
  --  (makeFunnyLambda "lambda"
  --    (mkExpr "applyInner" (Data.makeApply hole
  --      (mkExpr "var" (Data.ExpressionGetVariable (
  --        Data.ParameterRef (Guid.fromString "lambda"))
  --      ))
  --    ))
  --  )
  --  [ makeFunnyLambda "" hole
  --  , mkExpr "" $ Data.makePi hole boolType
  --  , hole, setType
  --  , mkExpr "" $ Data.makeApply funnyFunc hole, boolType
  --  , funnyFunc, funnyFuncType
  --  , funnyFuncType, setType, intType, setType, setType, setType, boolType, setType, setType, setType
  --  , hole, intType
  --  , hole, mkExpr "" $ Data.makePi hole intType
  --  , mkExpr "" . Data.ExpressionGetVariable . Data.ParameterRef $ Guid.fromString "lambda", hole
  --  ]
  ]
  where
    makeFunnyLambda g =
      mkExpr g .
      Data.makeLambda hole .
      mkExpr "applyFunny" .
      Data.makeApply funnyFunc
    funnyFunc =
      mkExpr "funnyFunc" . Data.ExpressionBuiltin $
      Data.Builtin funnyFuncName funnyFuncType
    funnyFuncName = Data.FFIName ["Test"] "funnyFunc"
    funnyFuncType = mkExpr "funcT" $ Data.makePi intType boolType
    testInfer name pureExpr result =
      testCase name .
      assertBool
        (unlines
         [ "Unexpected result expr:"
         , showExpressionWithInferred typedExpr
         , showExpressionWithInferred result
         ]) $ compareInferred typedExpr result
      where
        typedExpr = inferResults . fst $ toExpression pureExpr
