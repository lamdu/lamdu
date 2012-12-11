{-# OPTIONS -Wall -Werror #-}
module InferTests (allTests) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Exception (evaluate)
import Control.Lens ((^.))
import Control.Monad (join, void)
import Control.Monad.Trans.State (runStateT, evalState)
import Data.Map ((!))
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.IRef (DefI)
import Lamdu.Data.Infer.Conflicts (inferWithConflicts)
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
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.Infer as Infer
import qualified Test.HUnit as HUnit

type InferResults def =
  Data.Expression def
  ( Data.Expression def ()
  , Data.Expression def ()
  )

mkInferredGetDef :: String -> InferResults (DefI t)
mkInferredGetDef name =
  mkInferredLeafSimple
  (Data.GetVariable . Data.DefinitionRef $ IRef.unsafeFromGuid g)
  (void (definitionTypes ! g))
  where
    g = Guid.fromString name

mkInferredGetParam :: String -> Data.Expression def () -> InferResults def
mkInferredGetParam name = mkInferredLeafSimple gv
  where
    gv = Data.GetVariable . Data.ParameterRef $ Guid.fromString name

inferResults :: Data.Expression (DefI t) (Infer.Inferred (DefI t)) -> InferResults (DefI t)
inferResults = fmap (void . Infer.iValue &&& void . Infer.iType)

showExpressionWithInferred :: InferResults (DefI t) -> String
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

compareInferred :: InferResults (DefI t) -> InferResults (DefI t) -> Bool
compareInferred x y =
  isJust $ Data.matchExpression f ((const . const) Nothing) x y
  where
    f (v0, t0) (v1, t1) =
      liftA2 (,) (matchI v0 v1) (matchI t0 t1)
    matchI = Data.matchExpression nop ((const . const) Nothing)
    nop () () = Just ()

mkInferredLeafSimple :: Data.Leaf def -> Data.Expression def () -> InferResults def
mkInferredLeafSimple leaf =
  mkInferredLeaf leaf . Data.pureExpression $ Data.ExpressionLeaf leaf

mkInferredLeaf ::
  Data.Leaf def -> Data.Expression def () -> Data.Expression def () ->
  InferResults def
mkInferredLeaf leaf val typ =
  mkInferredNode val typ $ Data.ExpressionLeaf leaf

mkInferredNode ::
  Data.Expression def () ->
  Data.Expression def () ->
  Data.ExpressionBody def (InferResults def) -> InferResults def
mkInferredNode iVal iType body =
  Data.Expression body (iVal, iType)

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int"
    (Data.pureExpression (Data.makeLiteralInteger 5)) $
    mkInferredLeafSimple (Data.LiteralInteger 5) intType
  , testInfer "simple apply"
    (pureApply [hole, hole]) $
    mkInferredNode hole hole $
    Data.makeApply
      (inferredHole (purePi "" hole hole))
      (inferredHole hole)
  , testInfer "simple pi"
    (purePi "pi" hole hole) $
    mkInferredNode
      (purePi "pi" hole hole)
      setType $
    makeNamedPi ""
      (inferredHole setType)
      (inferredHole setType)
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply"
  (pureApply [pureGetDef "IntToBoolFunc", hole]) $
  mkInferredNode
    (pureApply [pureGetDef "IntToBoolFunc", hole])
    (pureGetDef "Bool") $
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
        (pureApply [pureGetDef ":", intType, five])
        endoListOfInt
      ( Data.makeApply
        ( mkInferredNode
            (pureApply [pureGetDef ":", intType])
            (purePi "" intType endoListOfInt)
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
    endoListOfInt = join (purePi "pi") $ listOf intType
    listOf x = pureApply [pureGetDef "List", x]
    applyWithHole h = pureApply [pureGetDef ":", h, five, pureGetParam "xs"]
    exprWithHoles h0 h1 =
      pureLambda "xs" (listOf h0) (applyWithHole h1)

applyOnVar :: HUnit.Test
applyOnVar =
  testInfer "apply on var"
  (makeFunnyLambda "lambda"
    (pureApply [ hole, pureGetParam "lambda" ] )
  ) $
  mkInferredNode
    (makeFunnyLambda "" hole)
    (purePi "" hole (pureGetDef "Bool")) $
  makeNamedLambda "lambda" (inferredHole setType) $
  mkInferredNode
    (pureApply [pureGetDef "IntToBoolFunc", hole])
    (pureGetDef "Bool") $
  Data.makeApply (mkInferredGetDef "IntToBoolFunc") $
  mkInferredNode
    hole
    intType $
  Data.makeApply
    (inferredHole (purePi "" hole intType)) $
  mkInferredLeafSimple
    ((Data.GetVariable . Data.ParameterRef . Guid.fromString) "lambda")
    hole

idTest :: HUnit.Test
idTest =
  testInfer "id test"
  applyIdInt $
  mkInferredNode
    applyIdInt
    (purePi "" intType intType) $
  Data.makeApply
    (mkInferredGetDef "id") $
    mkInferredLeafSimple Data.IntegerType setType

monomorphRedex :: HUnit.Test
monomorphRedex =
  testInfer "foo = f (λ~ x -> (λ~ -> x) _) where f ~:(a:Set -> _ -> a) = _"
  expr .
  mkInferredNode inferredExpr hole $
  Data.makeApply
    ( mkInferredNode (bodyLambda True) bodyLambdaType $
      makeNamedLambda "f"
        (mkInferredLeaf Data.Hole fType setType) $
      mkInferredNode (body True) hole $
      Data.makeApply
        (mkInferredGetParam "f" fType) $
      mkInferredNode (fArg True) (fArgType True) $
      makeNamedLambda "b"
        (mkInferredLeaf Data.Hole setType setType) $
      mkInferredNode xToX (fArgInnerType True) $
      makeNamedLambda "x"
        (mkInferredLeaf Data.Hole (pureGetParam "b") setType) $
      mkInferredNode (pureGetParam "x") (pureGetParam "b") $
      Data.makeApply
        ( mkInferredNode cToX (purePi "" hole (pureGetParam "b")) $
          makeNamedLambda "c"
            (mkInferredLeafSimple Data.Hole setType) $
          mkInferredGetParam "x" $ pureGetParam "b"
        ) $
      mkInferredLeafSimple Data.Hole hole
    ) $
  mkInferredNode (fExpr True) fType $
  makeNamedLambda "fArg"
    ( mkInferredNode (fArgType True) setType $
      makeNamedPi "b"
        (mkInferredLeafSimple Data.Set setType) $
      mkInferredNode (fArgInnerType True) setType $
      makeNamedPi "x"
        (mkInferredLeaf Data.Hole (pureGetParam "b") setType) $
      mkInferredGetParam "b" setType
    ) $
  mkInferredLeafSimple Data.Hole hole
  where
    expr = pureApply [bodyLambda False, fExpr False]
    inferredExpr = pureApply [fExpr True, fArg True]
    bodyLambda isInferred =
      pureLambda "f" (if isInferred then fType else hole) $
      body isInferred
    fExpr isInferred = pureLambda "" (fArgType isInferred) hole
    fArgType isInferred = purePi "b" setType $ fArgInnerType isInferred
    fArgInnerType False = purePi "" hole $ pureGetParam "b"
    fArgInnerType True = purePi "" (pureGetParam "b") $ pureGetParam "b"
    body isInferred = pureApply [pureGetParam "f", fArg isInferred]
    fArg False =
        pureLambda "b" hole .
        pureLambda "x" hole $
        pureApply [cToX, hole]
    fArg True = pureLambda "b" setType xToX
    cToX = pureLambda "c" hole $ pureGetParam "x"
    xToX = pureLambda "x" (pureGetParam "b") $ pureGetParam "x"
    bodyLambdaType = purePi "" fType hole
    fType = purePi "" (fArgType True) hole

fOfXIsFOf5 :: HUnit.Test
fOfXIsFOf5 =
  testInfer "f x = f 5"
  (pureLambda "x" hole (pureApply [getRecursiveDef, five])) $
  mkInferredNode
    (pureLambda "" intType (pureApply [getRecursiveDef, five]))
    (purePi "" intType hole) $
  makeNamedLambda "x"
    (mkInferredLeaf Data.Hole intType setType) $
  mkInferredNode
    (pureApply [getRecursiveDef, five])
    hole $
  Data.makeApply
    (mkInferredLeafSimple
      (Data.GetVariable (Data.DefinitionRef defI))
      (purePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

five :: Data.Expression (DefI t) ()
five = Data.pureExpression $ Data.makeLiteralInteger 5

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi"
  (pureApply [hole, five]) $
  mkInferredNode
    hole
    hole $
  Data.makeApply
    (inferredHole (purePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int"
  (pureApply
    [ pureGetDef "id"
    , hole
    , Data.pureExpression $ Data.makeLiteralInteger 5
    ]
  ) $
  mkInferredNode
    (pureApply
      [ pureGetDef "id"
      , intType
      , Data.pureExpression $ Data.makeLiteralInteger 5
      ]
    )
    intType $
  Data.makeApply
    (mkInferredNode
      (pureApply [pureGetDef "id", intType])
      (purePi "" intType intType)
      (Data.makeApply
        (mkInferredGetDef "id")
        ((mkInferredNode intType setType . Data.ExpressionLeaf) Data.Hole)
      )
    ) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnHole :: HUnit.Test
idOnHole =
  testInfer "id hole"
  (pureApply [pureGetDef "id", hole]) .
  mkInferredNode
    (pureApply [pureGetDef "id", hole])
    (purePi "" hole hole) .
  Data.makeApply
    (mkInferredGetDef "id") $
  inferredHole setType

forceMono :: HUnit.Test
forceMono =
  testInfer "id (id _ _)"
  (pureApply [pureGetDef "id", idHoleHole]) .
  mkInferredNode
    (pureApply [pureGetDef "id", idSetHole])
    (purePi "" idSetHole idSetHole) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredNode
    idSetHole
    setType $
  Data.makeApply
    (mkInferredNode
      idSet
      (purePi "" setType setType)
      (Data.makeApply
        (mkInferredGetDef "id")
        (mkInferredLeaf Data.Hole setType setType)
      )
    ) $
  mkInferredLeafSimple Data.Hole setType
  where
    idHole = pureApply [pureGetDef "id", hole]
    idHoleHole = pureApply [idHole, hole]
    idSet = pureApply [pureGetDef "id", setType]
    idSetHole = pureApply [idSet, hole]

inferredHole :: Data.Expression def () -> InferResults def
inferredHole = mkInferredLeafSimple Data.Hole

inferredSetType :: InferResults def
inferredSetType = mkInferredLeafSimple Data.Set setType

-- | depApply (t : Set) (rt : t -> Set) (f : (d : t) -> rt d) (x : t) = f x
depApply :: HUnit.Test
depApply =
  testInfer "dep apply"
  -- expr:
  tLambda .
  -- expected result:
  mkInferredNode  tLambda  tPi  . makeNamedLambda "t"  inferredSetType .
  mkInferredNode rtLambda rtPi  . makeNamedLambda "rt" inferredRTType .
  mkInferredNode  fLambda  fPi  . makeNamedLambda "f"  inferredFParamType .
  mkInferredNode  xLambda  xPi  . makeNamedLambda "x"  inferredXParamType .
  mkInferredNode fOfX (rtAppliedTo "x") $ Data.makeApply inferredF inferredX
  where
    inferredF = mkInferredGetParam "f" fParamType
    inferredX = mkInferredGetParam "x" xParamType
    inferredT = mkInferredGetParam "t" setType
    inferredRT = mkInferredGetParam "rt" rtParamType
    inferredRTType =
      mkInferredNode rtParamType setType $
      makeNamedPi "" inferredT inferredSetType
    inferredFParamType =
      mkInferredNode fParamType setType . makeNamedPi "d" inferredT .
      mkInferredNode (rtAppliedTo "d") setType .
      Data.makeApply inferredRT .
      mkInferredGetParam "d" $
      pureGetParam "t"
    inferredXParamType = inferredT
    makeLamPi name paramType (body, bodyType) =
      ( pureLambda name paramType body
      , purePi name paramType bodyType
      )
    (tLambda, tPi) = makeLamPi "t" setType rt
    rt@(rtLambda, rtPi) = makeLamPi "rt" rtParamType f
    f@(fLambda, fPi) = makeLamPi "f" fParamType x
    x@(xLambda, xPi) = makeLamPi "x" xParamType (fOfX, rtAppliedTo "x")
    fOfX = Data.pureExpression . Data.makeApply (pureGetParam "f") $ pureGetParam "x"
    rtParamType = purePi "" (pureGetParam "t") setType
    fParamType =
      purePi "d" (pureGetParam "t") $ rtAppliedTo "d"
    xParamType = pureGetParam "t"
    rtAppliedTo name =
      Data.pureExpression . Data.makeApply (pureGetParam "rt") $ pureGetParam name

getLambdaBody :: Data.Expression (DefI t) a -> Data.Expression (DefI t) a
getLambdaBody e =
  x
  where
    Data.ExpressionLambda (Data.Lambda _ _ x) = e ^. Data.eValue

getPiResult :: Data.Expression (DefI t) a -> Data.Expression (DefI t) a
getPiResult e =
  x
  where
    Data.ExpressionPi (Data.Lambda _ _ x) = e ^. Data.eValue

getLambdaParamType :: Data.Expression (DefI t) a -> Data.Expression (DefI t) a
getLambdaParamType e =
  x
  where
    Data.ExpressionLambda (Data.Lambda _ x _) = e ^. Data.eValue

getApplyFunc :: Data.Expression (DefI t) a -> Data.Expression (DefI t) a
getApplyFunc e =
  x
  where
    Data.ExpressionApply (Data.Apply x _) = e ^. Data.eValue

getApplyArg :: Data.Expression (DefI t) a -> Data.Expression (DefI t) a
getApplyArg e =
  x
  where
    Data.ExpressionApply (Data.Apply _ x) = e ^. Data.eValue

testCase :: String -> HUnit.Assertion -> HUnit.Test
testCase name = HUnit.TestLabel name . HUnit.TestCase

testResume ::
  String -> Data.Expression (DefI t) () ->
  Data.Expression (DefI t) () ->
  (Data.Expression (DefI t) (Infer.Inferred (DefI t)) ->
   Data.Expression (DefI t) (Infer.Inferred (DefI t))) ->
  HUnit.Test
testResume name newExpr testExpr extract =
  testCase name $
  let
    (tExpr, inferContext) = doInfer_ testExpr
  in
    void . evaluate . (`runStateT` inferContext) $
    doInferM ((Infer.iPoint . Lens.view Data.ePayload . extract) tExpr) newExpr

applyIdInt :: Data.Expression (DefI t) ()
applyIdInt =
  Data.pureExpression
  (Data.makeApply
    (pureGetDef "id")
    intType
  )

-- {g, x} =>
-- \(g:hole) -> IntToBoolFunc x
makeFunnyLambda ::
  String ->
  Data.Expression (DefI t) () ->
  Data.Expression (DefI t) ()
makeFunnyLambda g =
  pureLambda g hole .
  Data.pureExpression .
  Data.makeApply (pureGetDef "IntToBoolFunc")

testInfer ::
  String -> Data.Expression (DefI t) () ->
  InferResults (DefI t) -> HUnit.Test
testInfer name pureExpr result =
  testCase name .
  assertBool
    (unlines
     [ "result expr:"  , showExpressionWithInferred inferredExpr
     , "expected expr:", showExpressionWithInferred result
     ]) $ compareInferred inferredExpr result
  where
    inferredExpr = inferResults . fst $ doInfer_ pureExpr

getRecursiveDef :: Data.Expression (DefI t) ()
getRecursiveDef =
  Data.pureExpression . Data.ExpressionLeaf . Data.GetVariable $ Data.DefinitionRef defI

resumptionTests :: [HUnit.Test]
resumptionTests =
  [ testResume "resume with pi"
    (purePi "" hole hole) hole id
  , testResume "resume infer in apply func"
    (pureGetDef "id") (pureApply [hole, hole]) getApplyFunc
  , testResume "resume infer in lambda body"
    (pureGetDef "id") (pureLambda "" hole hole) getLambdaBody
  , testResume "resume infer to get param 1 of 2"
    (pureGetParam "a")
    ((pureLambda "a" hole . pureLambda "b" hole) hole)
    (getLambdaBody . getLambdaBody)
  , testResume "resume infer to get param 2 of 2"
    (pureGetParam "b")
    ((pureLambda "a" hole . pureLambda "b" hole) hole)
    (getLambdaBody . getLambdaBody)
  , testResume "bad a b:Set f = f a {b}"
    (pureGetParam "b")
    ((pureLambda "a" hole .
      pureLambda "b" setType .
      pureLambda "f" hole)
     (pureApply [pureGetParam "f", pureGetParam "a", hole]))
    (getApplyArg . getLambdaBody . getLambdaBody . getLambdaBody)
  , testCase "ref to the def on the side" $
    let
      (exprD, inferContext) =
        doInfer_ $ pureLambda "" hole hole
      body = getLambdaBody exprD
      scope = Infer.nScope . Infer.iPoint $ body ^. Data.ePayload
      exprR = (`evalState` inferContext) $ do
        node <- Infer.newNodeWithScope scope
        doInferM_ node getRecursiveDef
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
      purePi "" hole hole
  ]

makeParameterRef :: String -> Data.Expression def ()
makeParameterRef = Data.pureExpression . Data.makeParameterRef . Guid.fromString

-- f     x    = x _ _
--   --------
--   (_ -> _)
--         ^ Set Bool here
failResumptionAddsRules :: HUnit.Test
failResumptionAddsRules =
  testCase "Resumption that adds rules and fails" .
  assertBool "Resumption should have failed" $ not success
  where
    (success, _iwc) = (`evalState` origInferContext) $
      -- TODO: Verify iwc has the right kind of conflict (or add
      -- different tests to do that)
      inferWithConflicts (doLoad resumptionValue) resumptionPoint
    resumptionValue = pureGetDef "Bool" -- <- anything but Pi
    resumptionPoint =
      ( Infer.iPoint . Lens.view Data.ePayload
      . getPiResult . getLambdaParamType
      ) origInferred
    (origInferred, origInferContext) = doInfer_ origExpr
    origExpr =
      pureLambda "x" (purePi "" hole hole) $
      pureApply [makeParameterRef "x", hole, hole]


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
  , monomorphRedex
  , inferPart
  , failResumptionAddsRules
  ]
  ++ resumptionTests

inferPreservesShapeProp :: Data.Expression (DefI t) () -> Property
inferPreservesShapeProp expr =
  case inferMaybe expr of
    Nothing -> property rejected
    Just (inferred, _) -> property (void inferred == expr)

qcProps :: [Test]
qcProps = [testProperty "infer preserves shape" inferPreservesShapeProp]

allTests :: [Test]
allTests = hUnitTestToTests hunitTests `mappend` qcProps
