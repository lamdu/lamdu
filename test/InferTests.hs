module InferTests (allTests) where

import Control.Applicative (liftA2)
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Map ((!))
import Data.Maybe (isJust)
import Test.HUnit (assertBool)
import Utils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Infer as Infer
import qualified Test.HUnit as HUnit

mkInferredGetDef :: String -> InferResults
mkInferredGetDef name =
  mkInferredLeafSimple
  (Data.GetVariable . Data.DefinitionRef $ IRef.unsafeFromGuid g)
  (definitionTypes ! g)
  where
    g = Guid.fromString name

mkInferredGetParam :: String -> Data.PureExpression -> InferResults
mkInferredGetParam name = mkInferredLeafSimple gv
  where
    gv = Data.GetVariable . Data.ParameterRef $ Guid.fromString name

type InferResults = Data.Expression (Data.PureExpression, Data.PureExpression)

inferResults :: Infer.Expression a -> InferResults
inferResults =
  fmap f
  where
    f inferred = (Infer.iValue inferred, Infer.iType inferred)

showExpressionWithInferred :: InferResults -> String
showExpressionWithInferred =
  List.intercalate "\n" . go
  where
    go inferredExpr =
      [ "Expr: " ++ show (Data.eGuid inferredExpr) ++ ":" ++ showStructure expr
      , "  IVal:  " ++ show val
      , "  IType: " ++ show typ
      ] ++
      (map ("  " ++) . Foldable.concat .
       fmap go) expr
      where
        expr = Data.eValue inferredExpr
        (val, typ) = Data.ePayload inferredExpr

compareInferred :: InferResults -> InferResults -> Bool
compareInferred x y =
  isJust $ Traversable.sequence =<< Data.matchExpression f x y
  where
    f (v0, t0) (v1, t1) =
      liftA2 (,) (matchI v0 v1) (matchI t0 t1)
    matchI = Data.matchExpression nop
    nop () () = ()

mkInferredLeafSimple :: Data.Leaf -> Data.PureExpression -> InferResults
mkInferredLeafSimple leaf =
  mkInferredLeaf leaf .
  Data.pureExpression (Guid.fromString "leaf") $
  Data.ExpressionLeaf leaf

mkInferredLeaf :: Data.Leaf -> Data.PureExpression -> Data.PureExpression -> InferResults
mkInferredLeaf leaf val typ =
  Data.Expression
  { Data.eGuid = Guid.fromString "leaf"
  , Data.eValue = Data.ExpressionLeaf leaf
  , Data.ePayload = (val, typ)
  }

mkInferredNode ::
  String -> Data.PureExpression -> Data.PureExpression ->
  Data.ExpressionBody InferResults -> InferResults
mkInferredNode g iVal iType body =
  Data.Expression (Guid.fromString g) body (iVal, iType)

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int"
    (mkExpr "5" (Data.ExpressionLeaf (Data.LiteralInteger 5))) $
    mkInferredLeafSimple (Data.LiteralInteger 5) intType
  , testInfer "simple apply"
    ((Data.canonizeGuids . makeApply) [hole, hole]) $
    mkInferredNode "" hole hole $
    Data.makeApply
      (inferredHole (makePi "" hole hole))
      (inferredHole hole)
  , testInfer "simple pi"
    (makePi "pi" hole hole) $
    mkInferredNode ""
      (makePi "pi" hole hole)
      setType $
    Data.makePi
      (inferredHole setType)
      (inferredHole setType)
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply"
  (makeApply [getDefExpr "IntToBoolFunc", hole]) $
  mkInferredNode ""
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    (getDefExpr "bool") $
  Data.makeApply
    (mkInferredGetDef "IntToBoolFunc")
    (inferredHole intType)

applyOnVar :: HUnit.Test
applyOnVar =
  testInfer "apply on var"
  (makeFunnyLambda "lambda"
    (makeApply
      [ hole
      , mkExpr "var" . Data.makeParameterRef $ Guid.fromString "lambda"
      ]
    )
  ) $
  mkInferredNode "lambda"
    (makeFunnyLambda "" hole)
    (makePi "" hole (getDefExpr "bool")) $
  Data.makeLambda (inferredHole setType) $
  mkInferredNode ""
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    (getDefExpr "bool") $
  Data.makeApply (mkInferredGetDef "IntToBoolFunc") $
  mkInferredNode ""
    hole
    intType $
  Data.makeApply
    (inferredHole (makePi "" hole intType)) $
  mkInferredLeafSimple
    ((Data.GetVariable . Data.ParameterRef . Guid.fromString) "lambda")
    hole

idTest :: HUnit.Test
idTest =
  testInfer "id test"
  applyIdInt $
  mkInferredNode ""
    applyIdInt
    (makePi "" intType intType) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredLeafSimple Data.IntegerType setType

fOfXIsFOf5 :: HUnit.Test
fOfXIsFOf5 =
  testInfer "f x = f 5"
  (makeLambda "x" hole (makeApply [getRecursiveDef, five])) $
  mkInferredNode "x"
    (makeLambda "" intType (makeApply [getRecursiveDef, five]))
    (makePi "" intType hole) $
  Data.makeLambda
    (mkInferredLeafSimple Data.IntegerType setType) $
  mkInferredNode ""
    (makeApply [getRecursiveDef, five])
    hole $
  Data.makeApply
    (mkInferredLeafSimple
      (Data.GetVariable (Data.DefinitionRef defI))
      (makePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

five :: Data.PureExpression
five = mkExpr "five" . Data.ExpressionLeaf $ Data.LiteralInteger 5

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi"
  (makeApply [hole, five]) $
  mkInferredNode ""
    hole
    hole $
  Data.makeApply
    (inferredHole (makePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int"
  ((Data.canonizeGuids . makeApply)
    [ getDefExpr "id"
    , hole
    , mkExpr "" . Data.ExpressionLeaf $ Data.LiteralInteger 5
    ]
  ) $
  mkInferredNode ""
    (makeApply
      [ getDefExpr "id"
      , intType
      , mkExpr "" . Data.ExpressionLeaf $ Data.LiteralInteger 5
      ]
    )
    intType $
  Data.makeApply
    (mkInferredNode ""
      (makeApply [getDefExpr "id", intType])
      (makePi "" intType intType)
      (Data.makeApply
        (mkInferredGetDef "id")
        ((mkInferredNode "" intType setType . Data.ExpressionLeaf) Data.Hole)
      )
    ) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnHole :: HUnit.Test
idOnHole =
  testInfer "id hole"
  (makeApply [getDefExpr "id", hole]) .
  mkInferredNode ""
    (makeApply [getDefExpr "id", hole])
    (makePi "" hole hole) .
  Data.makeApply
    (mkInferredGetDef "id") $
  inferredHole setType

forceMono :: HUnit.Test
forceMono =
  testInfer "id (id _ _)"
  (makeApply [getDefExpr "id", idHoleHole]) .
  mkInferredNode ""
    (makeApply [getDefExpr "id", idSetHole])
    (makePi "" idSetHole idSetHole) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredNode ""
    idSetHole
    setType $
  Data.makeApply
    (mkInferredNode ""
      idSet
      (makePi "" setType setType)
      (Data.makeApply
        (mkInferredGetDef "id")
        (mkInferredLeaf Data.Hole setType setType)
      )
    ) $
  mkInferredLeafSimple Data.Hole setType
  where
    idHole = makeApply [getDefExpr "id", hole]
    idHoleHole = Data.canonizeGuids $ makeApply [idHole, hole]
    idSet = makeApply [getDefExpr "id", setType]
    idSetHole = Data.canonizeGuids $ makeApply [idSet, hole]

inferredHole :: Data.PureExpression -> InferResults
inferredHole = mkInferredLeafSimple Data.Hole

-- | depApply =  \(t : Set) -> \(rt : t -> Set) -> \(f : (d : t) -> rt d) -> \(x : t) -> f x
depApply :: HUnit.Test
depApply =
  testInfer "dep apply"
  -- expr:
  tLambda .
  -- expected result:
  mkInferredNode "t"  tLambda  tPi  . Data.makeLambda inferredSetType .
  mkInferredNode "rt" rtLambda rtPi . Data.makeLambda inferredRTType .
  mkInferredNode "f"  fLambda  fPi  . Data.makeLambda inferredFParamType .
  mkInferredNode "x"  xLambda  xPi  . Data.makeLambda inferredXParamType .
  mkInferredNode "" fOfX (rtAppliedTo "x") $ Data.makeApply inferredF inferredX
  where
    inferredF = mkInferredGetParam "f" fParamType
    inferredX = mkInferredGetParam "x" xParamType
    inferredT = mkInferredGetParam "t" setType
    inferredRT = mkInferredGetParam "rt" rtParamType
    inferredSetType = mkInferredLeafSimple Data.Set setType
    inferredRTType =
      mkInferredNode "" rtParamType setType $
      Data.makePi inferredT inferredSetType
    inferredFParamType =
      mkInferredNode "d" fParamType setType . Data.makePi inferredT .
      mkInferredNode "" (rtAppliedTo "d") setType .
      Data.makeApply inferredRT .
      mkInferredGetParam "d" $
      getParamExpr "t"
    inferredXParamType = inferredT
    makeLamPi name paramType (body, bodyType) =
      ( makeLambda name paramType body
      , makePi name paramType bodyType
      )
    (tLambda, tPi) = makeLamPi "t" setType rt
    rt@(rtLambda, rtPi) = makeLamPi "rt" rtParamType f
    f@(fLambda, fPi) = makeLamPi "f" fParamType x
    x@(xLambda, xPi) = makeLamPi "x" xParamType (fOfX, rtAppliedTo "x")
    fOfX = mkExpr "" . Data.makeApply (getParamExpr "f") $ getParamExpr "x"
    rtParamType = makePi "" (getParamExpr "t") setType
    fParamType =
      makePi "d" (getParamExpr "t") $ rtAppliedTo "d"
    xParamType = getParamExpr "t"
    rtAppliedTo name =
      mkExpr "" . Data.makeApply (getParamExpr "rt") $ getParamExpr name

getLambdaBody :: Data.Expression a -> Data.Expression a
getLambdaBody e =
  x
  where
    Data.ExpressionLambda (Data.Lambda _ x) = Data.eValue e

getApplyFunc :: Data.Expression a -> Data.Expression a
getApplyFunc e =
  x
  where
    Data.ExpressionApply (Data.Apply x _) = Data.eValue e

getApplyArg :: Data.Expression a -> Data.Expression a
getApplyArg e =
  x
  where
    Data.ExpressionApply (Data.Apply _ x) = Data.eValue e

testCase :: String -> HUnit.Assertion -> HUnit.Test
testCase name = HUnit.TestLabel name . HUnit.TestCase

testResume ::
  String -> Data.PureExpression -> Data.PureExpression ->
  (Infer.Expression () -> Data.Expression (Infer.Inferred a)) ->
  HUnit.Test
testResume name newExpr testExpr extract =
  testCase name $
  let
    (tExpr, refMap) = doInfer testExpr
  in
    void . evaluate $
    doInferM refMap ((Infer.iPoint . Data.ePayload . extract) tExpr) Nothing (Just tExpr) newExpr

applyIdInt :: Data.PureExpression
applyIdInt =
  mkExpr ""
  (Data.makeApply
    (getDefExpr "id")
    intType
  )

makeFunnyLambda :: String -> Data.PureExpression -> Data.PureExpression
makeFunnyLambda g =
  makeLambda g hole .
  mkExpr "applyFunny" .
  Data.makeApply (getDefExpr "IntToBoolFunc")

testInfer ::
  String -> Data.PureExpression ->
  InferResults -> HUnit.Test
testInfer name pureExpr result =
  testCase name .
  assertBool
    (unlines
     [ "result expr:"  , showExpressionWithInferred inferredExpr
     , "expected expr:", showExpressionWithInferred result
     ]) $ compareInferred inferredExpr result
  where
    inferredExpr = inferResults . fst $ doInfer pureExpr

getRecursiveDef :: Data.PureExpression
getRecursiveDef =
  mkExpr "getRecursiveDef" . Data.ExpressionLeaf . Data.GetVariable $ Data.DefinitionRef defI

resumptionTests :: [HUnit.Test]
resumptionTests =
  [ testResume "resume with pi"
    (makePi "" hole hole) hole id
  , testResume "resume infer in apply func"
    (getDefExpr "id") (Data.canonizeGuids (makeApply [hole, hole])) getApplyFunc
  , testResume "resume infer in lambda body"
    (getDefExpr "id") (makeLambda "" hole hole) getLambdaBody
  , testResume "resume infer to get param 1 of 2"
    (getParamExpr "a")
    ((makeLambda "a" hole . makeLambda "b" hole) hole)
    (getLambdaBody . getLambdaBody)
  , testResume "resume infer to get param 2 of 2"
    (getParamExpr "b")
    ((makeLambda "a" hole . makeLambda "b" hole) hole)
    (getLambdaBody . getLambdaBody)
  , testResume "bad a b:Set f = f a {b}"
    (getParamExpr "b")
    ((makeLambda "a" hole .
      makeLambda "b" setType .
      makeLambda "f" hole)
     (Data.canonizeGuids (makeApply [getParamExpr "f", getParamExpr "a", hole])))
    (getApplyArg . getLambdaBody . getLambdaBody . getLambdaBody)
  , testCase "ref to the def on the side" $
    let
      (exprD, refMap) = doInfer $ makeLambda "" hole hole
      Data.ExpressionLambda (Data.Lambda _ body) = Data.eValue exprD
      scope = Infer.nScope . Infer.iPoint $ Data.ePayload body
      (exprR, _) =
        uncurry doInferM (Infer.newNodeWithScope scope refMap) Nothing Nothing
        getRecursiveDef
      resultD = inferResults exprD
      resultR = inferResults exprR
    in
      assertBool (unlines
        [ showExpressionWithInferred resultD
        , show scope
        , showExpressionWithInferred resultR
        ]) .
      compareInferred resultR .
      mkInferredLeafSimple (Data.GetVariable (Data.DefinitionRef defI)) $
      makePi "" hole hole
  ]

allTests :: HUnit.Test
allTests =
  HUnit.TestList $
  simpleTests ++
  [ applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnHole
  , depApply
  , forceMono
  , fOfXIsFOf5
  ] ++
  resumptionTests
