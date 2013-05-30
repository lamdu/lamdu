{-# OPTIONS -Wall -Werror -fno-warn-missing-signatures #-}
module InferTests (allTests) where

import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (evalState)
import Data.Monoid (Monoid(..))
import InferAssert
import InferCombinators
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.Infer.Conflicts (inferWithConflicts)
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
import qualified Test.HUnit as HUnit

simpleTests =
  [ testInfer "literal int" $ literalInteger 5
  , testInfer "simple apply" $
    holeWithInferredType (hole --> hole) $$ hole
  , testInfer "simple pi" $
    holeWithInferredType set -->
    holeWithInferredType set
  ]

applyIntToBoolFuncWithHole =
  testInfer "apply" $
  getDef "IntToBoolFunc" $$ holeWithInferredType integerType

inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  lambda "xs" listInts $ \xs ->
  getDef ":" $$ asHole integerType $$ literalInteger 5 $$ xs
  where
    listInts = listOf (asHole integerType)

applyOnVar =
  testInfer "apply on var" $
  lambda "x" (holeWithInferredType set) $ \x ->
  getDef "IntToBoolFunc" $$
  (holeWithInferredType (hole --> integerType) $$ x)

idTest = testInfer "id test" $ getDef "id" $$ integerType

inferFromOneArgToOther =
  testInfer "f = \\ a b (x:Map _ _) (y:Map a b) -> if {_ x y}" $
  lambda "a" (asHole set) $ \a ->
  lambda "b" (asHole set) $ \b ->
  let mkMapType f = getDef "Map" $$: [f a, f b] in
  lambda "x" (mkMapType asHole) $ \x ->
  lambda "y" (mkMapType id) $ \y ->
  getDef "if" $$ asHole (mkMapType id) $$:
  [holeWithInferredType (getDef "Bool"), x, y]

monomorphRedex =
  testInfer "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Set -> _ -> a) = _" $
  whereItem "f" (lambda "" fArgType (const hole)) $ \f ->
  f $$
  lambda "b" (asHole set)
  (\b ->
   lambda "x" (asHole b) $ \x ->
   lambda "c" (holeWithInferredType set) (const x) $$ hole)
  where
    -- (a:Set -> _[=a] -> a)
    fArgType = piType "a" set $ \a -> asHole a --> a

fOfXIsFOf5 =
  testInfer "f x = f 5" $
  lambda "" (asHole integerType) $ \_ ->
  recurse (integerType --> hole) $$ literalInteger 5

argTypeGoesToPi =
  testInfer "arg type goes to pi" $
  holeWithInferredType (integerType --> hole) $$ literalInteger 5

idOnAnInt =
  testInfer "id on an int" $
  getDef "id" $$ asHole integerType $$ literalInteger 5

idOnHole = testInfer "id hole" $ getDef "id" $$ holeWithInferredType set

forceMono =
  testInfer "id (id _ _)" $
  getDef "id" $$ (getDef "id" $$ asHole set $$ holeWithInferredType set)

-- | depApply (t : Set) (rt : t -> Set) (f : (d : t) -> rt d) (x : t) = f x
depApply =
  testInfer "dep apply" $
  lambda "t" set $ \t ->
  lambda "rt" (t --> set) $ \rt ->
  lambda "f" (piType "d" t (\d -> rt $$ d)) $ \f ->
  lambda "x" t $ \x -> f $$ x

applyFunc :: Lens.Traversal' (Expression def a) (Expression def a)
applyFunc = ExprLens.exprApply . Expr.applyFunc
applyArg :: Lens.Traversal' (Expression def a) (Expression def a)
applyArg = ExprLens.exprApply . Expr.applyArg
lamParamType :: Lens.Traversal' (Expression def a) (Expression def a)
lamParamType = ExprLens.exprLam . Expr.lambdaParamType
lamBody :: Lens.Traversal' (Expression def a) (Expression def a)
lamBody = ExprLens.exprLam . Expr.lambdaResult

testGroup title = HUnit.TestLabel title . HUnit.TestList

resumptionTests = testGroup "type infer resume" $
  [ testResume "{hole->pi}"
    hole id (hole --> hole)
  , testResume "{hole->id} hole"
    (hole $$ hole) applyFunc (getDef "id")
  , testResume "\\_:hole -> {hole->id}"
    (lambda "" hole (const hole)) lamBody (getDef "id")
  ] ++
  let
    lambdaAB = lambda "a" hole . const . lambda "b" hole . const $ hole
    lambdaABBody :: Lens.Traversal' (Expression def a) (Expression def a)
    lambdaABBody = lamBody . lamBody
  in
  [ testResume "\\a:hole -> \\b:hole -> {hole->a}"
    lambdaAB lambdaABBody (getParam "a" hole)
  , testResume "\\a:hole -> \\b:hole -> {hole->b}"
    lambdaAB lambdaABBody (getParam "b" hole)
  ] ++
  [ testResume "\\a:hole -> \\b:set -> \\f:hole -> f a {hole->b}"
    (lambda "a" hole $ \a ->
     lambda "b" set $ \_ ->
     lambda "f" hole $ \f -> f $$ a $$ hole)
    (lamBody . lamBody . lamBody . applyArg)
    (getParam "b" hole)
  , testCase "ref to the def on the side" $
    let
      (exprD, inferContext) =
        doInfer_ $ lambda "" hole (const hole)
      scope = exprD ^?! lamBody . Expr.ePayload . Lens.to (Infer.nScope . Infer.iPoint)
      exprR = (`evalState` inferContext) $ do
        node <- Infer.newNodeWithScope scope
        doInferM_ node getRecursiveDef
      resultR = inferResults exprR
    in assertCompareInferred resultR $ recurse (hole --> hole)
  ]

-- f     x    = x _ _
--   --------
--   (_ -> _)
--         ^ Set Bool here
failResumptionAddsRules =
  testCase "Resumption that adds rules and fails" .
  assertBool "Resumption should have failed" $ not success
  where
    (success, _iwc) = (`evalState` origInferContext) $
      -- TODO: Verify iwc has the right kind of conflict (or add
      -- different tests to do that)
      inferWithConflicts (doLoad resumptionValue) resumptionPoint
    resumptionValue = getDef "Bool" -- <- anything but Pi
    resumptionPoint =
      origInferred ^?! lamParamType . lamBody . Expr.ePayload . Lens.to Infer.iPoint
    (origInferred, origInferContext) =
      doInfer_ . lambda "x" (hole --> hole) $
      \x -> x $$ hole $$ hole

emptyRecordTest =
  testGroup "empty record"
  [ testInfer "type infer" $ record Type []
  , testInfer "val infer" $ record Val []
  ]

recordTest =
  testInfer "f a x:a = {x" $
  lambda "a" set $ \a ->
  lambda "x" a $ \x ->
  record Val
  [ (tag fieldGuid, x) ]
  where
    fieldGuid = Guid.fromString "field"

-- TODO: Test should verify that ImplicitVariables puts the restricted
-- form (b->c) in the right place.
addImplicitCurriedApply2Test =
  testCase "implicitCurriedApply2: \\f -> f ? ?" $
  inferWVAssertion (expr hole) wvExpr
  where
    expr a =
      lambda "f" (asHole (a --> hole)) $ \f ->
      setInferredType (hole --> hole) (f $$ holeWithInferredType a) $$ hole
    wvExpr = lambda "a" (asHole set) expr

uncurry2Test =
  testCase "uncurry2: \\params:{?->?->?, x:?, y:?} -> f x y   WV: \\a b c params:{f:?{a->b->c} x:?a y:?b} -> f x y : c" $
  inferWVAssertion (expr iset iset iset) wvExpr
  where
    iset = holeWithInferredType set
    expr a b c =
      lambdaRecord "params"
      [ ("f", asHole (b --> a --> c))
      , ("x", asHole b)
      , ("y", asHole a)
      ] $ \[f, x, y] ->
      f $$ x $$ y
    wvExpr =
      lambda "a" (asHole set) $ \a ->
      lambda "b" (asHole set) $ \b ->
      lambda "c" (asHole set) $ \c ->
      expr b c a

implicitVarTests =
  testGroup "add implicit variables"
  [ addImplicitCurriedApply2Test
  , uncurry2Test
  ]

inferRecordValTest =
  testInfer "id ({:Set) <hole> does not infer { val" $
  getDef "id" $$ rec $$ setInferredType rec hole
  where
    rec = record Type []

inferReplicateOfReplicate =
  testInfer "replicate <hole> (replicate <hole> 1) 2" $
  replicat (listOf integerType)
  (replicat integerType
   (literalInteger 1)
   (literalInteger 3))
  (literalInteger 2)
  where
    replicat typ x y =
      getDef "replicate" $$ asHole typ $$: [ x, y ]

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
  , inferReplicateOfReplicate
  , implicitVarTests
  , resumptionTests
  ]

inferPreservesShapeProp :: PureExprDefI t -> Property
inferPreservesShapeProp expr =
  case inferMaybe expr of
    Nothing -> property rejected
    Just (inferred, _) -> property (void inferred == expr)

qcProps = [testProperty "infer preserves shape" inferPreservesShapeProp]

allTests = hUnitTestToTests hunitTests `mappend` qcProps
