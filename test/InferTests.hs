{-# OPTIONS -Wall -Werror #-}
module InferTests (allTests) where

import Control.Lens.Operators
import Control.Monad (void)
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
    holeWithInferredType (hole --> hole) $$ hole
  , testInfer "simple pi" $
    holeWithInferredType setType -->
    holeWithInferredType setType
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply" $
  getDef "IntToBoolFunc" $$ holeWithInferredType integerType

inferPart :: HUnit.Test
inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  lambda "xs" listInts $ \xs ->
  getDef ":" $$ asHole integerType $$ integer 5 $$ xs
  where
    listInts = listOf (asHole integerType)

applyOnVar :: HUnit.Test
applyOnVar =
  testInfer "apply on var" $
  lambda "x" (holeWithInferredType setType) $ \x ->
  getDef "IntToBoolFunc" $$
  (holeWithInferredType (hole --> integerType) $$ x)

idTest :: HUnit.Test
idTest = testInfer "id test" $ getDef "id" $$ integerType

inferFromOneArgToOther :: HUnit.Test
inferFromOneArgToOther =
  testInfer "f = \\ a b (x:Map _ _) (y:Map a b) -> if {_ x y}" $
  lambda "a" (asHole setType) $ \a ->
  lambda "b" (asHole setType) $ \b ->
  let mkMapType f = getDef "Map" $$ f a $$ f b in
  lambda "x" (mkMapType asHole) $ \x ->
  lambda "y" (mkMapType id) $ \y ->
  getDef "if" $$ asHole (mkMapType id) $$:
  [holeWithInferredType (getDef "Bool"), x, y]

monomorphRedex :: HUnit.Test
monomorphRedex =
  testInfer "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Set -> _ -> a) = _" $
  whereItem "f" (lambda "" fArgType (const hole)) $ \f ->
  f $$
  (lambda "b" (asHole setType) $ \b ->
   lambda "x" (asHole b) $ \x ->
   lambda "c" (holeWithInferredType setType) (\_ -> x) $$ hole)
  where
    -- (a:Set -> _[=a] -> a)
    fArgType = piType "a" setType $ \a -> asHole a --> a

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
    (holeWithInferredType (integerType --> hole)) $
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
  holeWithInferredType setType

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
  iexpr  tLambda  tPi  . namedLambda "t"  setType .
  iexpr rtLambda rtPi  . namedLambda "rt" inferredRTType .
  iexpr  fLambda  fPi  . namedLambda "f"  inferredFParamType .
  iexpr  xLambda  xPi  . namedLambda "x"  inferredXParamType .
  iexpr fOfX (rtAppliedTo "x") $ ExprUtil.makeApply inferredF inferredX
  where
    inferredF = getParamPure "f" fParamType
    inferredX = getParamPure "x" xParamType
    inferredT = getParamPure "t" pureSet
    inferredRT = getParamPure "rt" rtParamType
    inferredRTType =
      iexpr rtParamType pureSet $
      namedPi "" inferredT setType
    inferredFParamType =
      iexpr fParamType pureSet . namedPi "d" inferredT .
      iexpr (rtAppliedTo "d") pureSet .
      ExprUtil.makeApply inferredRT .
      getParamPure "d" $
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
  namedLambda "x" (getParamPure "a" pureSet) $
  iexpr recVal recType $
  Expr.BodyRecord $
  Expr.Record Val
  [( simple fieldTagBody tagType
   , getParamPure "x" (pureGetParam "a")
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
  getDef "id" $$ record Type [] $$ asHole (record Val [])

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
