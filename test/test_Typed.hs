import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Identity (Identity(..))
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Maybe (isJust)
import Data.Store.Guid (Guid)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import qualified Test.Framework as TestFramework

setType :: Data.PureExpression
setType = mkExpr "set" $ Data.ExpressionLeaf Data.Set

intType :: Data.PureExpression
intType =
  mkExpr "int" $ Data.ExpressionLeaf Data.IntegerType

makeDefinitionRef :: Guid -> Data.ExpressionBody a
makeDefinitionRef = Data.ExpressionLeaf . Data.GetVariable . Data.DefinitionRef . IRef.unsafeFromGuid

boolType :: Data.PureExpression
boolType = mkExpr "bool" . makeDefinitionRef $ Guid.fromString "Bool"

intToBool :: Data.PureExpression
intToBool = mkExpr "intToBool" $ Data.makePi intType boolType

intToBoolFuncGetVar :: Data.Leaf
intToBoolFuncGetVar =
  Data.GetVariable . Data.DefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString "IntToBoolFunc"

intToBoolFunc :: Data.PureExpression
intToBoolFunc = mkExpr "bool" $ Data.ExpressionLeaf intToBoolFuncGetVar

definitionTypes :: Map Guid Data.PureExpression
definitionTypes =
  Map.fromList $ (map . first) Guid.fromString
  [ ("Bool", setType)
  , ("IntToBoolFunc", intToBool)
  ]

toExpression ::
  Data.PureExpression ->
  (Typed.Expression (), Typed.RefMap)
toExpression =
  runIdentity . Typed.inferFromEntity loader Nothing
  where
    loader = Typed.Loader (Identity . (definitionTypes !) . IRef.guid)

mkExpr ::
  String -> Data.ExpressionBody Data.PureExpression ->
  Data.PureExpression
mkExpr = Data.pureExpression . Guid.fromString

hole :: Data.PureExpression
hole = mkExpr "hole" $ Data.ExpressionLeaf Data.Hole

type InferResults = Data.Expression (Typed.InferredExpr, Typed.InferredExpr)

inferResults :: Typed.Expression s -> InferResults
inferResults =
  fmap f
  where
    f inferred = (Typed.iValue inferred, Typed.iType inferred)

showExpressionWithInferred :: InferResults -> String
showExpressionWithInferred =
  List.intercalate "\n" . go
  where
    showInferred x =
      case x ^. Typed.rErrors of
      [] -> show $ x ^. Typed.rExpression
      _ -> "ERROR: " ++ show x
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
    matchI r0 r1 = do
      guard $ on (==) (length . (^. Typed.rErrors)) r0 r1
      liftA2 (,)
        (sequence (on (zipWith matchPure) (^. Typed.rErrors) r0 r1))
        (on matchPure (^. Typed.rExpression) r0 r1)
    matchPure = Data.matchExpression nop
    nop () () = ()

mkInferredLeaf :: Data.Leaf -> Data.PureExpression -> InferResults
mkInferredLeaf leaf typ =
  Data.Expression
  { Data.eGuid = Guid.fromString "leaf"
  , Data.eValue = Data.ExpressionLeaf leaf
  , Data.ePayload =
      ( mkInferredExpr . Data.pureExpression g $ Data.ExpressionLeaf leaf
      , mkInferredExpr typ
      )
  }
  where
    g = Guid.fromString "leaf"

mkInferredExpr :: Data.PureExpression -> Typed.InferredExpr
mkInferredExpr = (`Typed.InferredExpr` [])

mkInferredNode ::
  String -> Data.PureExpression -> Data.PureExpression ->
  Data.ExpressionBody InferResults -> InferResults
mkInferredNode g iVal iType body =
  Data.Expression (Guid.fromString g) body (mkInferredExpr iVal, mkInferredExpr iType)

main :: IO ()
main = TestFramework.defaultMain
  [ testInfer "literal int"
    (mkExpr "5" (Data.ExpressionLeaf (Data.LiteralInteger 5))) $
    mkInferredLeaf (Data.LiteralInteger 5) intType
  , testInfer "simple apply"
    (mkExpr "apply" (Data.makeApply hole hole)) $
    mkInferredNode "" hole hole $
    Data.makeApply
      (mkInferredLeaf Data.Hole (mkExpr "" (Data.makePi hole hole)))
      (mkInferredLeaf Data.Hole hole)
  , testInfer "apply"
    (mkExpr "apply" (Data.makeApply intToBoolFunc hole)) $
    mkInferredNode ""
      (mkExpr "" (Data.makeApply intToBoolFunc hole))
      boolType $
    Data.makeApply
      (mkInferredLeaf intToBoolFuncGetVar intToBool)
      (mkInferredLeaf Data.Hole intType)
  , testInfer "apply on var"
    (makeFunnyLambda "lambda"
      (mkExpr "applyInner"
        (Data.makeApply
          hole
          (mkExpr "var" (Data.makeParameterRef (Guid.fromString "lambda")))
        )
      )
    ) $
    mkInferredNode "lambda"
      (makeFunnyLambda "" hole)
      (mkExpr "" (Data.makePi hole boolType)) $
    Data.makeLambda (mkInferredLeaf Data.Hole setType) $
    mkInferredNode ""
      (mkExpr "" (Data.makeApply intToBoolFunc hole))
      boolType $
    Data.makeApply (mkInferredLeaf intToBoolFuncGetVar intToBool) $
    mkInferredNode ""
      hole
      intType $
    Data.makeApply
      (mkInferredLeaf Data.Hole (mkExpr "" (Data.makePi hole intType))) $
    mkInferredLeaf
      (Data.GetVariable (Data.ParameterRef (Guid.fromString "lambda")))
      hole
  ]
  where
    makeFunnyLambda g =
      mkExpr g .
      Data.makeLambda hole .
      mkExpr "applyFunny" .
      Data.makeApply intToBoolFunc
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
