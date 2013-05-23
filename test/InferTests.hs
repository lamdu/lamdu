{-# OPTIONS -Wall -Werror #-}
module InferTests (allTests) where

import Control.Lens.Operators
import Control.Monad (join, void)
import Control.Monad.Trans.State (evalState)
import Data.Monoid (Monoid(..))
import InferAssert
import InferCombinators
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.Infer.Conflicts (inferWithConflicts)
import Lamdu.Data.Expression.Utils (pureHole, pureSet, pureIntegerType)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)
import Test.QuickCheck (Property)
import Test.QuickCheck.Property (property, rejected)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Test.HUnit as HUnit

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int" $ integer 5
  , testInfer "simple apply" $
    iexpr pureHole pureHole $
    ExprUtil.makeApply
      (holeWithInferredType (purePi "" pureHole pureHole))
      (holeWithInferredType pureHole)
  , testInfer "simple pi" $
    iexpr
      (purePi "pi" pureHole pureHole)
      pureSet $
    namedPi ""
      (holeWithInferredType pureSet)
      (holeWithInferredType pureSet)
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply" $
  iexpr
    (pureApply [pureGetDef "IntToBoolFunc", pureHole])
    (pureGetDef "Bool") $
  ExprUtil.makeApply
    (getDef "IntToBoolFunc")
    (holeWithInferredType pureIntegerType)

inferPart :: HUnit.Test
inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  iexpr
    (exprWithHoles pureIntegerType pureIntegerType)
    endoListOfInt $
  namedLambda "xs"
    (iexpr (listOf pureIntegerType) pureSet
      (ExprUtil.makeApply
        (getDef "List")
        (iexpr pureIntegerType pureSet bodyHole)
      )
    ) $
  iexpr
    (applyWithHole pureIntegerType)
    (listOf pureIntegerType) $
  ExprUtil.makeApply
    ( iexpr
        (pureApply [pureGetDef ":", pureIntegerType, five])
        endoListOfInt
      ( ExprUtil.makeApply
        ( iexpr
            (pureApply [pureGetDef ":", pureIntegerType])
            (purePi "" pureIntegerType endoListOfInt)
          (ExprUtil.makeApply
            (getDef ":")
            (iexpr pureIntegerType pureSet bodyHole)
          )
        )
        (integer 5)
      )
    ) $
  getParam "xs" $ listOf pureIntegerType
  where
    endoListOfInt = join (purePi "pi") $ listOf pureIntegerType
    listOf x = pureApply [pureGetDef "List", x]
    applyWithHole h = pureApply [pureGetDef ":", h, five, pureGetParam "xs"]
    exprWithHoles h0 h1 =
      pureLambda "xs" (listOf h0) (applyWithHole h1)

applyOnVar :: HUnit.Test
applyOnVar =
  testInfer "apply on var" $
  iexpr
    (funnyLambda "" pureHole)
    (purePi "" pureHole (pureGetDef "Bool")) $
  namedLambda "lambda" (holeWithInferredType pureSet) $
  iexpr
    (pureApply [pureGetDef "IntToBoolFunc", pureHole])
    (pureGetDef "Bool") $
  ExprUtil.makeApply (getDef "IntToBoolFunc") $
  iexpr
    pureHole
    pureIntegerType $
  ExprUtil.makeApply
    (holeWithInferredType (purePi "" pureHole pureIntegerType)) $
  simple
    (ExprLens.bodyParameterRef # Guid.fromString "lambda")
    pureHole

idTest :: HUnit.Test
idTest =
  testInfer "id test" $
  iexpr
    (ExprLens.pureExpr # ExprUtil.makeApply (pureGetDef "id") pureIntegerType)
    (purePi "" pureIntegerType pureIntegerType) $
  ExprUtil.makeApply
    (getDef "id") $
    simple bodyIntegerType pureSet

inferFromOneArgToOther :: HUnit.Test
inferFromOneArgToOther =
  testInfer "f = \\ a b (x:Map _ _) (y:Map a b) -> if {_ x y}" .
  iexpr expr (purePi "a" pureSet lamBType) $
  namedLambda "a"
    (iexpr pureSet pureSet bodyHole) $
  iexpr lamB lamBType $
  namedLambda "b"
    (iexpr pureSet pureSet bodyHole) $
  iexpr lamX lamXType $
  namedLambda "x"
    ( iexpr typeOfX pureSet $
      ExprUtil.makeApply
        ( iexpr typeOfX' setToSet $
          ExprUtil.makeApply
            (getDef "Map") $
          iexpr (pureGetParam "a") pureSet bodyHole
        ) $
      iexpr (pureGetParam "b") pureSet bodyHole
    ) $
  iexpr lamY lamYType $
  namedLambda "y"
    ( iexpr typeOfX pureSet $
      ExprUtil.makeApply
        ( iexpr typeOfX' setToSet $
          ExprUtil.makeApply
            (getDef "Map") $
          getParam "a" pureSet
        ) $
      getParam "b" pureSet
    ) $
  iexpr body typeOfX $
  ExprUtil.makeApply
    ( iexpr body1 body1Type $
      ExprUtil.makeApply
        (getDef "if") $
      iexpr typeOfX pureSet bodyHole
    ) $
  iexpr ifParams ifParamsType $
  Expr.BodyRecord $ Expr.Record Val
  [ (tag t0, simple bodyHole (pureGetDef "Bool"))
  , (tag t1, getParam "x" typeOfX)
  , (tag t2, getParam "y" typeOfX)
  ]
  where
    [t0, t1, t2] = defParamTags "if"
    expr = pureLambda "a" pureSet lamB
    lamB = pureLambda "b" pureSet lamX
    lamX = pureLambda "x" typeOfX lamY
    lamY = pureLambda "y" typeOfX body
    body = pureApply [body1, ifParams]
    body1 = pureApply [pureGetDef "if", typeOfX]
    ifParams =
      ExprUtil.pureExpression . Expr.BodyRecord $
      Expr.Record Val
      [ (pureTag # t0, pureHole)
      , (pureTag # t1, pureGetParam "x")
      , (pureTag # t2, pureGetParam "y")
      ]
    ifParamsType =
      ExprUtil.pureExpression . Expr.BodyRecord $
      Expr.Record Type
      [ (pureTag # t0, pureGetDef "Bool")
      , (pureTag # t1, typeOfX)
      , (pureTag # t2, typeOfX)
      ]
    pureTag = ExprLens.pureExpr . ExprLens.bodyTag
    body1Type = purePi "body1" ifParamsType typeOfX
    typeOfX =
      pureApply
      [ typeOfX'
      , pureGetParam "b"
      ]
    typeOfX' =
      pureApply
      [ pureGetDef "Map"
      , pureGetParam "a"
      ]
    setToSet = purePi "setToSet" pureSet pureSet
    lamBType = purePi "b" pureSet lamXType
    lamXType = purePi "x" typeOfX lamYType
    lamYType = purePi "y" typeOfX typeOfX

monomorphRedex :: HUnit.Test
monomorphRedex =
  testInfer "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Set -> _ -> a) = _" .
  iexpr inferredExpr pureHole $
  ExprUtil.makeApply
    ( iexpr (bodyLambda True) bodyLambdaType $
      namedLambda "f"
        (iexpr fType pureSet bodyHole) $
      iexpr (body True) pureHole $
      ExprUtil.makeApply
        (getParam "f" fType) $
      iexpr (fArg True) (fArgType True) $
      namedLambda "b"
        (iexpr pureSet pureSet bodyHole) $
      iexpr xToX (fArgInnerType True) $
      namedLambda "x"
        (iexpr (pureGetParam "b") pureSet bodyHole) $
      iexpr (pureGetParam "x") (pureGetParam "b") $
      ExprUtil.makeApply
        ( iexpr cToX (purePi "" pureHole (pureGetParam "b")) $
          namedLambda "c"
            (simple bodyHole pureSet) $
          getParam "x" $ pureGetParam "b"
        ) $
      simple bodyHole pureHole
    ) $
  iexpr (fExpr True) fType $
  namedLambda "fArg"
    ( iexpr (fArgType True) pureSet $
      namedPi "b"
        (simple bodySet pureSet) $
      iexpr (fArgInnerType True) pureSet $
      namedPi "x"
        (iexpr (pureGetParam "b") pureSet bodyHole) $
      getParam "b" pureSet
    ) $
  simple bodyHole pureHole
  where
    inferredExpr = pureApply [fExpr True, fArg True]
    bodyLambda isInferred =
      pureLambda "f" (if isInferred then fType else pureHole) $
      body isInferred
    fExpr isInferred = pureLambda "" (fArgType isInferred) pureHole
    fArgType isInferred = purePi "b" pureSet $ fArgInnerType isInferred
    fArgInnerType False = purePi "" pureHole $ pureGetParam "b"
    fArgInnerType True = purePi "" (pureGetParam "b") $ pureGetParam "b"
    body isInferred = pureApply [pureGetParam "f", fArg isInferred]
    fArg False =
      pureLambda "b" pureHole .
      pureLambda "x" pureHole $
      pureApply [cToX, pureHole]
    fArg True = pureLambda "b" pureSet xToX
    cToX = pureLambda "c" pureHole $ pureGetParam "x"
    xToX = pureLambda "x" (pureGetParam "b") $ pureGetParam "x"
    bodyLambdaType = purePi "" fType pureHole
    fType = purePi "" (fArgType True) pureHole

fOfXIsFOf5 :: HUnit.Test
fOfXIsFOf5 =
  testInfer "f x = f 5" $
  iexpr
    (pureLambda "" pureIntegerType (pureApply [getRecursiveDef, five]))
    (purePi "" pureIntegerType pureHole) $
  namedLambda "x"
    (iexpr pureIntegerType pureSet bodyHole) $
  iexpr
    (pureApply [getRecursiveDef, five])
    pureHole $
  ExprUtil.makeApply
    (simple
      (ExprLens.bodyDefinitionRef # recursiveDefI)
      (purePi "" pureIntegerType pureHole)) $
  integer 5

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi" $
  iexpr
    pureHole
    pureHole $
  ExprUtil.makeApply
    (holeWithInferredType (purePi "" pureIntegerType pureHole)) $
  integer 5

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int" $
  iexpr
    (pureApply
      [ pureGetDef "id"
      , pureIntegerType
      , five
      ]
    )
    pureIntegerType $
  ExprUtil.makeApply
    (iexpr
      (pureApply [pureGetDef "id", pureIntegerType])
      (purePi "" pureIntegerType pureIntegerType)
      (ExprUtil.makeApply
        (getDef "id")
        ((iexpr pureIntegerType pureSet . Expr.BodyLeaf) Expr.Hole)
      )
    ) $
  integer 5

idOnHole :: HUnit.Test
idOnHole =
  testInfer "id hole" .
  iexpr
    (pureApply [pureGetDef "id", pureHole])
    (purePi "" pureHole pureHole) .
  ExprUtil.makeApply
    (getDef "id") $
  holeWithInferredType pureSet

forceMono :: HUnit.Test
forceMono =
  testInfer "id (id _ _)" .
  iexpr
    (pureApply [pureGetDef "id", idSetHole])
    (purePi "" idSetHole idSetHole) $
  ExprUtil.makeApply
    (getDef "id") $
  iexpr
    idSetHole
    pureSet $
  ExprUtil.makeApply
    (iexpr
      idSet
      (purePi "" pureSet pureSet)
      (ExprUtil.makeApply
        (getDef "id")
        (iexpr pureSet pureSet bodyHole)
      )
    ) $
  simple bodyHole pureSet
  where
    idSet = pureApply [pureGetDef "id", pureSet]
    idSetHole = pureApply [idSet, pureHole]

-- | depApply (t : Set) (rt : t -> Set) (f : (d : t) -> rt d) (x : t) = f x
depApply :: HUnit.Test
depApply =
  testInfer "dep apply" .
  -- expected result:
  iexpr  tLambda  tPi  . namedLambda "t"  inferredSetType .
  iexpr rtLambda rtPi  . namedLambda "rt" inferredRTType .
  iexpr  fLambda  fPi  . namedLambda "f"  inferredFParamType .
  iexpr  xLambda  xPi  . namedLambda "x"  inferredXParamType .
  iexpr fOfX (rtAppliedTo "x") $ ExprUtil.makeApply inferredF inferredX
  where
    inferredF = getParam "f" fParamType
    inferredX = getParam "x" xParamType
    inferredT = getParam "t" pureSet
    inferredRT = getParam "rt" rtParamType
    inferredRTType =
      iexpr rtParamType pureSet $
      namedPi "" inferredT inferredSetType
    inferredFParamType =
      iexpr fParamType pureSet . namedPi "d" inferredT .
      iexpr (rtAppliedTo "d") pureSet .
      ExprUtil.makeApply inferredRT .
      getParam "d" $
      pureGetParam "t"
    inferredXParamType = inferredT
    lamPi name paramType (body, bodyType) =
      ( pureLambda name paramType body
      , purePi name paramType bodyType
      )
    (tLambda, tPi) = lamPi "t" pureSet rt
    rt@(rtLambda, rtPi) = lamPi "rt" rtParamType f
    f@(fLambda, fPi) = lamPi "f" fParamType x
    x@(xLambda, xPi) = lamPi "x" xParamType (fOfX, rtAppliedTo "x")
    fOfX = ExprUtil.pureExpression . ExprUtil.makeApply (pureGetParam "f") $ pureGetParam "x"
    rtParamType = purePi "" (pureGetParam "t") pureSet
    fParamType =
      purePi "d" (pureGetParam "t") $ rtAppliedTo "d"
    xParamType = pureGetParam "t"
    rtAppliedTo name =
      ExprUtil.pureExpression . ExprUtil.makeApply (pureGetParam "rt") $ pureGetParam name

-- {g, x} =>
-- \(g:hole) -> IntToBoolFunc x
funnyLambda ::
  String ->
  PureExprDefI t ->
  PureExprDefI t
funnyLambda g =
  pureLambda g pureHole .
  ExprUtil.pureExpression .
  ExprUtil.makeApply (pureGetDef "IntToBoolFunc")

getRecursiveDef :: PureExprDefI t
getRecursiveDef =
  ExprUtil.pureExpression $ Lens.review ExprLens.bodyDefinitionRef recursiveDefI

resumptionTests :: [HUnit.Test]
resumptionTests =
  [ testResume "resume with pi"
    (purePi "" pureHole pureHole) pureHole id
  , testResume "resume infer in apply func"
    (pureGetDef "id") (pureApply [pureHole, pureHole]) (ExprLens.exprApply . Expr.applyFunc)
  , testResume "resume infer in lambda body"
    (pureGetDef "id") (pureLambda "" pureHole pureHole) lamBody
  , testResume "resume infer to get param 1 of 2"
    (pureGetParam "a")
    ((pureLambda "a" pureHole . pureLambda "b" pureHole) pureHole) (lamBody . lamBody)
  , testResume "resume infer to get param 2 of 2"
    (pureGetParam "b")
    ((pureLambda "a" pureHole . pureLambda "b" pureHole) pureHole) (lamBody . lamBody)
  , testResume "bad a b:Set f = f a {b}"
    (pureGetParam "b")
    ((pureLambda "a" pureHole .
      pureLambda "b" pureSet .
      pureLambda "f" pureHole)
     (pureApply [pureGetParam "f", pureGetParam "a", pureHole]))
    (lamBody . lamBody . lamBody . ExprLens.exprApply . Expr.applyArg)
  , testCase "ref to the def on the side" $
    let
      (exprD, inferContext) =
        doInfer_ $ pureLambda "" pureHole pureHole
      Just body = exprD ^? lamBody
      scope = Infer.nScope . Infer.iPoint $ body ^. Expr.ePayload
      exprR = (`evalState` inferContext) $ do
        node <- Infer.newNodeWithScope scope
        doInferM_ node getRecursiveDef
      resultR = inferResults exprR
    in
      assertCompareInferred resultR .
      simple (ExprLens.bodyDefinitionRef # recursiveDefI) $
      purePi "" pureHole pureHole
  ]
  where
    lamBody :: Lens.Traversal' (Expression def a) (Expression def a)
    lamBody = ExprLens.exprLam . Expr.lambdaResult

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
    Just pl =
      origInferred ^?
      ExprLens.exprLam . Expr.lambdaParamType .
      ExprLens.exprLam . Expr.lambdaResult .
      Expr.ePayload
    resumptionPoint = Infer.iPoint pl
    (origInferred, origInferContext) = doInfer_ origExpr
    origExpr =
      pureLambda "x" (purePi "" pureHole pureHole) $
      pureApply [pureParameterRef "x", pureHole, pureHole]

emptyRecordTest :: HUnit.Test
emptyRecordTest =
  testInfer "empty record type infer" $
  iexpr emptyRecordType pureSet emptyRecordTypeBody
  where
    emptyRecordType = ExprUtil.pureExpression emptyRecordTypeBody
    emptyRecordTypeBody =
      Expr.BodyRecord $ Expr.Record Type mempty

tagType :: Expression def ()
tagType = ExprUtil.pureExpression $ Expr.BodyLeaf Expr.TagType

recordTest :: HUnit.Test
recordTest =
  testInfer "f a x:a = {x" $
  iexpr lamA piA $
  namedLambda "a" (simple bodySet pureSet) $
  iexpr lamX piX $
  namedLambda "x" (getParam "a" pureSet) $
  iexpr recVal recType $
  Expr.BodyRecord $
  Expr.Record Val
  [( simple fieldTagBody tagType
   , getParam "x" (pureGetParam "a")
   )]
  where
    lamA =
      pureLambda "a" pureSet lamX
    lamX =
      pureLambda "x" (pureGetParam "a") recVal
    recVal =
      ExprUtil.pureExpression $
      rec Val $
      pureGetParam "x"
    fieldTagBody = ExprLens.bodyTag # fieldGuid
    rec k =
      Expr.BodyRecord . Expr.Record k .
      (:[]) . (,) (ExprLens.pureExpr # fieldTagBody)
    fieldGuid = Guid.fromString "field"
    piA =
      purePi "a" pureSet piX
    piX =
      purePi "x" (pureGetParam "a") recType
    recType =
      ExprUtil.pureExpression $
      rec Type $
      pureGetParam "a"

inferRecordValTest :: HUnit.Test
inferRecordValTest =
  testInfer "id ({:Set) <hole> infers { val" $
  apply [getDef "id", record Type [], inferredVal (record Val [])]

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
  , inferFromOneArgToOther
  , depApply
  , forceMono
  , fOfXIsFOf5
  , monomorphRedex
  , inferPart
  , failResumptionAddsRules
  , emptyRecordTest
  , recordTest
  , inferRecordValTest
  ]
  ++ resumptionTests

inferPreservesShapeProp :: PureExprDefI t -> Property
inferPreservesShapeProp expr =
  case inferMaybe expr of
    Nothing -> property rejected
    Just (inferred, _) -> property (void inferred == expr)

qcProps :: [Test]
qcProps = [testProperty "infer preserves shape" inferPreservesShapeProp]

allTests :: [Test]
allTests = hUnitTestToTests hunitTests `mappend` qcProps
