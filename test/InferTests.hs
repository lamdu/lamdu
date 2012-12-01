module InferTests (allTests) where

import Control.Applicative (liftA2)
import Control.Exception (evaluate)
import Control.Lens ((^.))
import Control.Monad (join, void)
import Data.Map ((!))
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import Editor.Data.Arbitrary () -- Arbitrary instance
import Editor.Data.Infer.Conflicts (inferWithConflicts)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)
import Test.QuickCheck (Property)
import Test.QuickCheck.Property (property, rejected)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Editor.Data as Data
import qualified Editor.Data.Infer as Infer
import qualified Test.HUnit as HUnit

mkInferredGetDef :: String -> InferResults
mkInferredGetDef name =
  mkInferredLeafSimple
  (Data.GetVariable . Data.DefinitionRef $ IRef.unsafeFromGuid g)
  (void (definitionTypes ! g))
  where
    g = Guid.fromString name

mkInferredGetParam :: String -> Data.Expression DefI () -> InferResults
mkInferredGetParam name = mkInferredLeafSimple gv
  where
    gv = Data.GetVariable . Data.ParameterRef $ Guid.fromString name

type InferResults =
  Data.Expression DefI
  ( Data.Expression DefI ()
  , Data.Expression DefI ()
  )

inferResults :: Data.Expression DefI (Infer.Inferred DefI a) -> InferResults
inferResults =
  fmap f
  where
    f inferred = (Infer.iValue inferred, Infer.iType inferred)

showExpressionWithInferred :: InferResults -> String
showExpressionWithInferred =
  List.intercalate "\n" . go
  where
    go inferredExpr =
      [ "Expr:" ++ showStructure expr
      , "  IVal:  " ++ show val
      , "  IType: " ++ show typ
      ] ++
      (map ("  " ++) . Foldable.concat .
       fmap go) expr
      where
        expr = inferredExpr ^. Data.eValue
        (val, typ) = inferredExpr ^. Data.ePayload

compareInferred :: InferResults -> InferResults -> Bool
compareInferred x y =
  isJust $ Data.matchExpression f ((const . const) Nothing) x y
  where
    f (v0, t0) (v1, t1) =
      liftA2 (,) (matchI v0 v1) (matchI t0 t1)
    matchI = Data.matchExpression nop ((const . const) Nothing)
    nop () () = Just ()

mkInferredLeafSimple :: Data.Leaf DefI -> Data.Expression DefI () -> InferResults
mkInferredLeafSimple leaf =
  mkInferredLeaf leaf . Data.pureExpression $ Data.ExpressionLeaf leaf

mkInferredLeaf ::
  Data.Leaf DefI -> Data.Expression DefI () -> Data.Expression DefI () -> InferResults
mkInferredLeaf leaf val typ =
  Data.Expression
  { Data._eValue = Data.ExpressionLeaf leaf
  , Data._ePayload = (val, typ)
  }

mkInferredNode ::
  Data.Expression DefI () ->
  Data.Expression DefI () ->
  Data.ExpressionBody DefI InferResults -> InferResults
mkInferredNode iVal iType body =
  Data.Expression body (iVal, iType)

makeNamedLambda :: String -> expr -> expr -> Data.ExpressionBody DefI expr
makeNamedLambda = Data.makeLambda . Guid.fromString

makeNamedPi :: String -> expr -> expr -> Data.ExpressionBody DefI expr
makeNamedPi = Data.makePi . Guid.fromString

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int"
    (Data.pureExpression (Data.ExpressionLeaf (Data.LiteralInteger 5))) $
    mkInferredLeafSimple (Data.LiteralInteger 5) intType
  , testInfer "simple apply"
    (makeApply [hole, hole]) $
    mkInferredNode hole hole $
    Data.makeApply
      (inferredHole (makePi "" hole hole))
      (inferredHole hole)
  , testInfer "simple pi"
    (makePi "pi" hole hole) $
    mkInferredNode
      (makePi "pi" hole hole)
      setType $
    makeNamedPi ""
      (inferredHole setType)
      (inferredHole setType)
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply"
  (makeApply [getDefExpr "IntToBoolFunc", hole]) $
  mkInferredNode
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    (getDefExpr "Bool") $
  Data.makeApply
    (mkInferredGetDef "IntToBoolFunc")
    (inferredHole intType)

inferPart :: HUnit.Test
inferPart =
  testInfer "foo (xs:List ?) = 5 : xs"
  (exprWithHoles hole hole) $
  mkInferredNode
    (exprWithHoles intType intType)
    endoListOfInt $
  makeNamedLambda "xs"
    (mkInferredNode (listOf intType) setType
      (Data.makeApply
        (mkInferredGetDef "List")
        (mkInferredLeaf Data.Hole intType setType)
      )
    ) $
  mkInferredNode
    (applyWithHole intType)
    (listOf intType) $
  Data.makeApply
    ( mkInferredNode
        (makeApply [getDefExpr ":", intType, five])
        endoListOfInt
      ( Data.makeApply
        ( mkInferredNode
            (makeApply [getDefExpr ":", intType])
            (makePi "" intType endoListOfInt)
          (Data.makeApply
            (mkInferredGetDef ":")
            (mkInferredLeaf Data.Hole intType setType)
          )
        )
        (mkInferredLeafSimple (Data.LiteralInteger 5) intType)
      )
    ) $
  mkInferredGetParam "xs" $ listOf intType
  where
    endoListOfInt = join (makePi "pi") $ listOf intType
    listOf x = makeApply [getDefExpr "List", x]
    applyWithHole h = makeApply [getDefExpr ":", h, five, getParamExpr "xs"]
    exprWithHoles h0 h1 =
      makeLambda "xs" (listOf h0) (applyWithHole h1)

applyOnVar :: HUnit.Test
applyOnVar =
  testInfer "apply on var"
  (makeFunnyLambda "lambda"
    (makeApply [ hole, getParamExpr "lambda" ] )
  ) $
  mkInferredNode
    (makeFunnyLambda "" hole)
    (makePi "" hole (getDefExpr "Bool")) $
  makeNamedLambda "lambda" (inferredHole setType) $
  mkInferredNode
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    (getDefExpr "Bool") $
  Data.makeApply (mkInferredGetDef "IntToBoolFunc") $
  mkInferredNode
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
  mkInferredNode
    applyIdInt
    (makePi "" intType intType) $
  Data.makeApply
    (mkInferredGetDef "id") $
    mkInferredLeafSimple Data.IntegerType setType

fOfXIsFOf5 :: HUnit.Test
fOfXIsFOf5 =
  testInfer "f x = f 5"
  (makeLambda "x" hole (makeApply [getRecursiveDef, five])) $
  mkInferredNode
    (makeLambda "" intType (makeApply [getRecursiveDef, five]))
    (makePi "" intType hole) $
  makeNamedLambda "x"
    (mkInferredLeaf Data.Hole intType setType) $
  mkInferredNode
    (makeApply [getRecursiveDef, five])
    hole $
  Data.makeApply
    (mkInferredLeafSimple
      (Data.GetVariable (Data.DefinitionRef defI))
      (makePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

five :: Data.Expression DefI ()
five = Data.pureExpression . Data.ExpressionLeaf $ Data.LiteralInteger 5

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi"
  (makeApply [hole, five]) $
  mkInferredNode
    hole
    hole $
  Data.makeApply
    (inferredHole (makePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int"
  (makeApply
    [ getDefExpr "id"
    , hole
    , Data.pureExpression . Data.ExpressionLeaf $ Data.LiteralInteger 5
    ]
  ) $
  mkInferredNode
    (makeApply
      [ getDefExpr "id"
      , intType
      , Data.pureExpression . Data.ExpressionLeaf $ Data.LiteralInteger 5
      ]
    )
    intType $
  Data.makeApply
    (mkInferredNode
      (makeApply [getDefExpr "id", intType])
      (makePi "" intType intType)
      (Data.makeApply
        (mkInferredGetDef "id")
        ((mkInferredNode intType setType . Data.ExpressionLeaf) Data.Hole)
      )
    ) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnHole :: HUnit.Test
idOnHole =
  testInfer "id hole"
  (makeApply [getDefExpr "id", hole]) .
  mkInferredNode
    (makeApply [getDefExpr "id", hole])
    (makePi "" hole hole) .
  Data.makeApply
    (mkInferredGetDef "id") $
  inferredHole setType

forceMono :: HUnit.Test
forceMono =
  testInfer "id (id _ _)"
  (makeApply [getDefExpr "id", idHoleHole]) .
  mkInferredNode
    (makeApply [getDefExpr "id", idSetHole])
    (makePi "" idSetHole idSetHole) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredNode
    idSetHole
    setType $
  Data.makeApply
    (mkInferredNode
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
    idHoleHole = makeApply [idHole, hole]
    idSet = makeApply [getDefExpr "id", setType]
    idSetHole = makeApply [idSet, hole]

inferredHole :: Data.Expression DefI () -> InferResults
inferredHole = mkInferredLeafSimple Data.Hole

-- | depApply =  \(t : Set) -> \(rt : t -> Set) -> \(f : (d : t) -> rt d) -> \(x : t) -> f x
depApply :: HUnit.Test
depApply =
  testInfer "dep apply"
  -- expr:
  tLambda .
  -- expected result:
  mkInferredNode  tLambda  tPi  . makeNamedLambda "t"  inferredSetType .
  mkInferredNode rtLambda rtPi . makeNamedLambda "rt" inferredRTType .
  mkInferredNode  fLambda  fPi  . makeNamedLambda "f"  inferredFParamType .
  mkInferredNode  xLambda  xPi  . makeNamedLambda "x"  inferredXParamType .
  mkInferredNode fOfX (rtAppliedTo "x") $ Data.makeApply inferredF inferredX
  where
    inferredF = mkInferredGetParam "f" fParamType
    inferredX = mkInferredGetParam "x" xParamType
    inferredT = mkInferredGetParam "t" setType
    inferredRT = mkInferredGetParam "rt" rtParamType
    inferredSetType = mkInferredLeafSimple Data.Set setType
    inferredRTType =
      mkInferredNode rtParamType setType $
      makeNamedPi "" inferredT inferredSetType
    inferredFParamType =
      mkInferredNode fParamType setType . makeNamedPi "d" inferredT .
      mkInferredNode (rtAppliedTo "d") setType .
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
    fOfX = Data.pureExpression . Data.makeApply (getParamExpr "f") $ getParamExpr "x"
    rtParamType = makePi "" (getParamExpr "t") setType
    fParamType =
      makePi "d" (getParamExpr "t") $ rtAppliedTo "d"
    xParamType = getParamExpr "t"
    rtAppliedTo name =
      Data.pureExpression . Data.makeApply (getParamExpr "rt") $ getParamExpr name

getLambdaBody :: Data.Expression DefI a -> Data.Expression DefI a
getLambdaBody e =
  x
  where
    Data.ExpressionLambda (Data.Lambda _ _ x) = e ^. Data.eValue

getPiResult :: Data.Expression DefI a -> Data.Expression DefI a
getPiResult e =
  x
  where
    Data.ExpressionPi (Data.Lambda _ _ x) = e ^. Data.eValue

getLambdaParamType :: Data.Expression DefI a -> Data.Expression DefI a
getLambdaParamType e =
  x
  where
    Data.ExpressionLambda (Data.Lambda _ x _) = e ^. Data.eValue

getApplyFunc :: Data.Expression DefI a -> Data.Expression DefI a
getApplyFunc e =
  x
  where
    Data.ExpressionApply (Data.Apply x _) = e ^. Data.eValue

getApplyArg :: Data.Expression DefI a -> Data.Expression DefI a
getApplyArg e =
  x
  where
    Data.ExpressionApply (Data.Apply _ x) = e ^. Data.eValue

testCase :: String -> HUnit.Assertion -> HUnit.Test
testCase name = HUnit.TestLabel name . HUnit.TestCase

testResume ::
  String -> Data.Expression DefI () ->
  Data.Expression DefI () ->
  (Data.Expression DefI (Infer.Inferred DefI ()) ->
   Data.Expression DefI (Infer.Inferred DefI a)) ->
  HUnit.Test
testResume name newExpr testExpr extract =
  testCase name $
  let
    (tExpr, inferContext) = doInfer testExpr
  in
    void . evaluate $
    doInferM inferContext
    ((Infer.iPoint . Lens.view Data.ePayload . extract) tExpr)
    newExpr

applyIdInt :: Data.Expression DefI ()
applyIdInt =
  Data.pureExpression
  (Data.makeApply
    (getDefExpr "id")
    intType
  )

-- {g, x} =>
-- \(g:hole) -> IntToBoolFunc x
makeFunnyLambda ::
  String ->
  Data.Expression DefI () ->
  Data.Expression DefI ()
makeFunnyLambda g =
  makeLambda g hole .
  Data.pureExpression .
  Data.makeApply (getDefExpr "IntToBoolFunc")

testInfer ::
  String -> Data.Expression DefI () ->
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

getRecursiveDef :: Data.Expression DefI ()
getRecursiveDef =
  Data.pureExpression . Data.ExpressionLeaf . Data.GetVariable $ Data.DefinitionRef defI

resumptionTests :: [HUnit.Test]
resumptionTests =
  [ testResume "resume with pi"
    (makePi "" hole hole) hole id
  , testResume "resume infer in apply func"
    (getDefExpr "id") (makeApply [hole, hole]) getApplyFunc
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
     (makeApply [getParamExpr "f", getParamExpr "a", hole]))
    (getApplyArg . getLambdaBody . getLambdaBody . getLambdaBody)
  , testCase "ref to the def on the side" $
    let
      (exprD, inferContext) =
        doInfer $ makeLambda "" hole hole
      body = getLambdaBody exprD
      scope = Infer.nScope . Infer.iPoint $ body ^. Data.ePayload
      (exprR, _) =
        uncurry doInferM
        (Infer.newNodeWithScope scope inferContext)
        getRecursiveDef
      resultD = inferResults exprD
      resultR = inferResults exprR
    in
      assertBool (unlines
        [ "Root expression inferred:"
        , showExpressionWithInferred resultD
        , "Scope:"
        , show scope
        , ""
        , showExpressionWithInferred resultR
        ]) .
      compareInferred resultR .
      mkInferredLeafSimple (Data.GetVariable (Data.DefinitionRef defI)) $
      makePi "" hole hole
  ]

makeParameterRef :: String -> Data.Expression def ()
makeParameterRef = Data.pureExpression . Data.makeParameterRef . Guid.fromString

-- f     x    = x _ _
--   --------
--   (_ -> _)
--         ^ Set Bool here
failResumptionAddsRules :: HUnit.Test
failResumptionAddsRules =
  testCase "Resumption that adds rules and fails" $
  assertBool "Resumption should have failed" (not success)
  where
    -- TODO: Verify iwc has the right kind of conflict (or add
    -- different tests to do that)
    (success, _, _iwc) =
      inferWithConflicts (doLoad resumptionValue)
      origRefMap resumptionPoint
    resumptionValue = getDefExpr "Bool" -- <- anything but Pi
    resumptionPoint =
      ( Infer.iPoint . Lens.view Data.ePayload
      . getPiResult . getLambdaParamType
      ) origInferred
    (origInferred, origRefMap) = doInfer origExpr
    origExpr =
      makeLambda "x" (makePi "" hole hole) $
      makeApply [makeParameterRef "x", hole, hole]

hunitTests :: HUnit.Test
hunitTests =
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
  , inferPart
  , failResumptionAddsRules
  ]
  ++ resumptionTests

inferPreservesShapeProp :: Data.Expression DefI () -> Property
inferPreservesShapeProp expr =
  case inferMaybe expr of
    Nothing -> property rejected
    Just (inferred, _) -> property (void inferred == expr)

qcProps :: [Test]
qcProps = [testProperty "infer preserves shape" inferPreservesShapeProp]

allTests :: [Test]
allTests = hUnitTestToTests hunitTests `mappend` qcProps
