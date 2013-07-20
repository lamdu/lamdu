{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror -fno-warn-missing-signatures #-}
module InferTests (allTests, factorialExpr, euler1Expr, solveDepressedQuarticExpr) where

import Control.Applicative
import Control.Lens.Operators
import Control.Monad (void)
import Data.Store.Guid (Guid)
import InferAssert
import InferCombinators
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property)
import Test.QuickCheck.Property (property, rejected)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer as Infer

simpleTests =
  [ testInfer "literal int" $ literalInteger 5
  , testInfer "simple apply" $
    holeWithInferredType (hole ~> hole) $$ hole
  , testInfer "simple pi" $
    holeWithInferredType set ~>
    holeWithInferredType set
  ]

applyIntToBoolFuncWithHole =
  testInfer "apply" $
  getDef "IntToBoolFunc" $$ holeWithInferredType integerType

inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  lambda "xs" listInts $ \xs ->
  getDef ":" $$ asHole integerType $$:
  [literalInteger 5, xs]
  where
    listInts = listOf (asHole integerType)

-- Depend on Rigidity:

applyOnVar =
  testInferAllowFail "No support for rigidity yet"
    "apply on var" $
  lambda "x" (holeWithInferredType set) $ \x ->
  getDef "IntToBoolFunc" $$
  (holeWithInferredType (hole ~> integerType) $$ x)

monomorphRedex =
  testInferAllowFail "No support for rigidity detection yet"
    "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Type -> _ -> a) = _" $
  whereItem "f" (lambda "" fArgType (const hole)) $ \f ->
  f $$
  lambda "b" (asHole set)
  (\b ->
   lambda "x" (asHole b) $ \x ->
   lambda "c" (holeWithInferredType set) (const x) $$ hole)
  where
    -- (a:Type -> _[=a] -> a)
    fArgType = piType "a" set $ \a -> asHole a ~> a

-- Solved in new_infer
idPreservesDependency =
  testInfer "5 + f _ where f x = id _{no inferred type}" $
  whereItem "f" (lambda "x" iset (const (getDef "id" $$ iset $$ hole))) $ \f ->
  getDef "+" $$ asHole integerType $$:
  [literalInteger 5, setInferredType integerType (f $$ hole)]
  where
    iset = holeWithInferredType set

idTest = testInfer "id test" $ getDef "id" $$ integerType

inferFromOneArgToOther =
  testInfer "f = \\ a (b:set) (c:a) -> if {True b c}" $
  lambda "a" set $ \a ->
  lambda "b" a $ \b ->
  lambda "c" a $ \c ->
  getDef "if" $$ a $$:
  [getDef "True", b, c]

inferFromOneArgToOtherMap =
  testInfer "f = \\ a b (c:List _ _) (d:Map a b) -> if {_ c d}" $
  lambda "a" (asHole set) $ \a ->
  lambda "b" (asHole set) $ \b ->
  let mkMapType f = getDef "Map" $$: [f a, f b] in
  lambda "c" (mkMapType asHole) $ \c ->
  lambda "d" (mkMapType id) $ \d ->
  getDef "if" $$ asHole (mkMapType id) $$:
  [holeWithInferredType (getDef "Bool"), c, d]

fOfXIsFOf5 =
  testInfer "f x = f 5" $
  lambda "" (asHole integerType) $ \_ ->
  recurse (integerType ~> hole) $$ literalInteger 5

argTypeGoesToPi =
  testInfer "arg type goes to pi" $
  holeWithInferredType (integerType ~> hole) $$ literalInteger 5

idOnAnInt =
  testInfer "id on an int" $
  getDef "id" $$ asHole integerType $$ literalInteger 5

holeTag = holeWithInferredType tagType

idOnARecord =
  testInfer "id ({:Type) <hole> does not infer { val" $
  getDef "id" $$ rec $$ holeWithInferredType rec
  where
    rec = record KType [(holeTag, integerType)]

idOnHole = testInfer "id hole" $ getDef "id" $$ holeWithInferredType set

forceMono =
  testInfer "id (id _ _)" $
  getDef "id" $$ (getDef "id" $$ asHole set $$ holeWithInferredType set)

-- | depApply (t : Type) (rt : t -> Type) (f : (d : t) -> rt d) (x : t) = f x
depApply =
  testInfer "dep apply" $
  lambda "t" set $ \t ->
  lambda "rt" (t ~> set) $ \rt ->
  lambda "f" (piType "d" t (\d -> rt $$ d)) $ \f ->
  lambda "x" t $ \x -> f $$ x

applyFunc :: Lens.Traversal' (Expression def a) (Expression def a)
applyFunc = ExprLens.exprApply . Expr.applyFunc
applyArg :: Lens.Traversal' (Expression def a) (Expression def a)
applyArg = ExprLens.exprApply . Expr.applyArg
lamParamType :: Kind -> Lens.Traversal' (Expression def a) (Expression def a)
lamParamType k = ExprLens.exprKindedLam k . Lens._2
lamResult :: Kind -> Lens.Traversal' (Expression def a) (Expression def a)
lamResult k = ExprLens.exprKindedLam k . Lens._3

resumptionTests =
  testGroup "type infer resume" $
  [ testResume "{hole->pi}"
    hole id (hole ~> hole)
  , testResume "{hole->id} hole"
    (hole $$ hole) applyFunc (getDef "id")
  , testResume "\\_:hole -> {hole->id}"
    (lambda "" hole (const hole)) (lamResult KVal) (getDef "id")
  ] ++
  let
    lambdaAB = lambda "a" hole . const . lambda "b" hole . const $ hole
    lambdaABBody :: Lens.Traversal' (Expression def a) (Expression def a)
    lambdaABBody = lamResult KVal . lamResult KVal
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
    (lamResult KVal . lamResult KVal . lamResult KVal . applyArg)
    (getParam "b" hole)
  , testCase "ref to the def on the side" $
    let
      resultR = assertSuccess . runNewContext $ do
        exprD <- inferDef $ infer =<< load (lambda "" hole (const hole))
        let scope = exprD ^?! lamResult KVal . Expr.ePayload . Lens._1 . Infer.stvScope
        exprR <- inferScope scope =<< load (recurse (hole ~> hole))
        deref $ fst <$> exprR
    in assertCompareInferred resultR (recurse (hole ~> hole))
  ]

failResumptionAddsRules =
  -- f     x    = x _ _
  --   --------
  --   (_ -> _)
  --         ^ Set Bool here
  testCase "Resumption that adds rules and fails" .
  runContextAssertion $ do
    rootInferred <- inferDef $ infer =<< load expr
    fmap verifyError . try $
      loadInferInto (rootInferred ^?! resumptionPoint) resumptionValue
  where
    verifyError :: Either Error () -> M ()
    verifyError (Left (InferError Infer.Mismatch {})) = return ()
    verifyError _ = error "Resumption did not fail!"
    expr = lambda "x" (hole ~> hole) $ \x -> x $$ hole $$ hole
    resumptionValue = getDef "Bool" -- <- anything but Pi
    resumptionPoint = lamParamType KVal . lamResult KType

emptyRecordTests =
  testGroup "empty record"
  [ testInfer "type infer" $ record KType []
  , testInfer "val infer" $ record KVal []
  ]

recordTest =
  testInfer "f a x:a = {x" $
  lambda "a" set $ \a ->
  lambda "x" a $ \x ->
  record KVal
  [ (tag fieldGuid, x) ]
  where
    fieldGuid = Guid.fromString "field"

-- Depend on ImplicitVariables:

-- -- TODO: Test should verify that ImplicitVariables puts the restricted
-- -- form (b->c) in the right place.
-- addImplicitCurriedApply2Test =
--   testCase "implicitCurriedApply2: \\f -> f ? ?" $
--   inferWVAssertion (expr hole) wvExpr
--   where
--     expr a =
--       lambda "f" (asHole (a ~> hole)) $ \f ->
--       setInferredType (hole ~> hole) (f $$ holeWithInferredType a) $$ hole
--     wvExpr = lambda "a" (asHole set) expr

-- uncurry2Test =
--   testCase "uncurry2: \\params:{?->?->?, x:?, y:?} -> f x y   WV: \\a b c params:{f:?{a->b->c} x:?a y:?b} -> f x y : c" $
--   inferWVAssertion (expr iset iset iset) wvExpr
--   where
--     iset = holeWithInferredType set
--     expr a b c =
--       lambdaRecord "params"
--       [ ("f", asHole (b ~> a ~> c))
--       , ("x", asHole b)
--       , ("y", asHole a)
--       ] $ \[f, x, y] ->
--       f $$ x $$ y
--     wvExpr =
--       lambda "a" (asHole set) $ \a ->
--       lambda "b" (asHole set) $ \b ->
--       lambda "c" (asHole set) $ \c ->
--       expr b c a

-- implicitVarTests =
--   testGroup "add implicit variables"
--   [ addImplicitCurriedApply2Test
--   , uncurry2Test
--   ]

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

infiniteTypeTests =
  testGroup "Infinite types"
  [ wrongRecurseMissingArg
  , getFieldWasntAllowed
  ]

getFieldWasntAllowed =
  testCase "map (\\x:_. #x#) {}:_" $
  assertResume topLevel pos $ getGuidParam param integerType
  where
    topLevel =
      getDef "map" $$ asHole recType $$ hole $$:
      [ getDef ":" $$ asHole recType $$:
        [ record KVal []
        , holeWithInferredType $ listOf recType
        ]
      , lambda "x" (asHole recType) $
        const hole
      ]
    pos :: Lens.Traversal' (Expression def a) (Expression def a)
    pos = lambdaPos . Lens._3
    lambdaPos :: Lens.Traversal' (Expression def a) (Guid, Expression def a, Expression def a)
    lambdaPos =
      applyArg .
      ExprLens.exprKindedRecordFields KVal . Lens.ix 1 . Lens._2 .
      ExprLens.exprKindedLam KVal
    param = topLevel ^?! lambdaPos . Lens._1
    recType = record KType []

wrongRecurseMissingArg =
  testCase "f x = f" .
  inferFailsAssertion "Infinite Type" isExpectedError $
  lambda "x" hole . const $ recurse hole
  where
    isExpectedError (InferError Infer.InfiniteExpression {}) = True
    isExpectedError _ = False

mapIdTest =
  testInfer "map id (5:_)" $
  getDef "map" $$ asHole integerType $$ asHole integerType $$:
  [ getDef ":" $$ asHole integerType $$:
    [ literalInteger 5
    , holeWithInferredType $ listOf integerType
    ]
  , getDef "id" $$ asHole integerType
  ]

factorialExpr =
  lambda "x" iInt $ \x ->
  getDef "if" $$ iInt $$:
  [ getDef "==" $$ iInt $$:
    [x, literalInteger 0]
  , literalInteger 1
  , getDef "*" $$ iInt $$:
    [ x
    , recurse (integerType ~> integerType) $$
      (getDef "-" $$ iInt $$: [x, literalInteger 1])
    ]
  ]
  where
    iInt = asHole integerType

euler1Expr =
  getDef "sum" $$ iInt $$
  ( getDef "filter" $$ iInt $$:
    [ getDef ".." $$: [literalInteger 1, literalInteger 1000]
    , lambda "x" iInt $ \x ->
      getDef "||" $$:
      [ getDef "==" $$ iInt $$:
        [ literalInteger 0, getDef "%" $$ iInt $$: [x, literalInteger 3] ]
      , getDef "==" $$ iInt $$:
        [ literalInteger 0, getDef "%" $$ iInt $$: [x, literalInteger 5] ]
      ]
    ]
  )
  where
    iInt = asHole integerType

-- Solve depressed quartic polynomial
solveDepressedQuarticExpr =
  lambdaRecord "params"
  [ ("e", asHole integerType)
  , ("d", asHole integerType)
  , ("c", asHole integerType)
  ] $ \[e, d, c] ->
  whereItem "solvePoly" ( getDef "id" $$ iListInt )
  $ \solvePoly ->
  whereItem "sqrts"
  ( lambda "x" iInt $ \x ->
    whereItem "r"
    ( getDef "sqrt" $$ iInt $$ x
    ) $ \r ->
    list [r, getDef "negate" $$ iInt $$ r]
  )
  $ \sqrts ->
  getDef "if" $$ iListInt $$:
  [ getDef "==" $$ iInt $$: [d, literalInteger 0]
  , getDef "concat" $$ iInt $$
    ( getDef "map" $$ iInt $$ iListInt $$:
      [ solvePoly $$ list [e, c, literalInteger 1]
      , sqrts
      ]
    )
  , getDef "concat" $$ iInt $$
    ( getDef "map" $$ iInt $$ iListInt $$:
      [ sqrts $$ (getDef "head" $$ iInt $$ (solvePoly $$ list
        [ getDef "negate" $$ iInt $$ (d %* d)
        , (c %* c) %- (literalInteger 4 %* e)
        , literalInteger 2 %* c
        , literalInteger 1
        ]))
      , lambda "x" iInt $ \x ->
        solvePoly $$ list
        [ (c %+ (x %* x)) %- (d %/ x)
        , literalInteger 2 %* x
        , literalInteger 2
        ]
      ]
    )
  ]
  where
    iInt = asHole integerType
    iListInt = asHole $ listOf integerType
    x %+ y = getDef "+" $$ iInt $$: [x, y]
    x %- y = getDef "-" $$ iInt $$: [x, y]
    x %* y = getDef "*" $$ iInt $$: [x, y]
    x %/ y = getDef "/" $$ iInt $$: [x, y]

joinMaybe =
  testInfer "\\x:_ -> caseMaybe x (empty=Nothing, just=\\x->x)" $
  lambda "x" (asHole (maybeOf (maybeOf iset))) $ \x ->
  getDef "caseMaybe" $$ maybeOf iset $$ asHole (maybeOf iset)
  $$:
  [ x
  , getDef "Nothing" $$ iset
  , lambda "item" (asHole (maybeOf iset)) id
  ]
  where
    iset = holeWithInferredType set

typeOfUndefined = piType "a" set id

scopeEscape =
  testCase "scope escape" .
  inferFailsAssertion "VarEscapesScope" isExpectedError $
  lambda "x" hole $ \x ->
  typedWhereItem "undef"
  typeOfUndefined (lambda "a" set (const x)) id
  where
    isExpectedError (InferError Infer.VarEscapesScope {}) = True
    isExpectedError _ = False

tagCompositeTests =
  testGroup "Composite Tags"
  [ testCase name $
    inferFailsAssertion "CompositeTag" isExpectedError expr
  | (name, expr) <- tests
  ]
  where
    isExpectedError (InferError Infer.CompositeTag {}) = True
    isExpectedError _ = False
    tests =
      [ ("in get field", getField hole (holeWithInferredType (hole ~> hole) $$ hole))
      , ("in record val field", record KVal [(hole $$ hole, hole)])
      , ("in record type field", record KVal [(hole $$ hole, hole)])
      ]

getFieldTests =
  testGroup "GetField tests"
  [ testGroup "missing fields"
    [ let
        isExpectedError (InferError Infer.GetMissingField {}) = True
        isExpectedError _ = False
      in
        testCase name .
        allowFailAssertion "No missing field logic yet" .
        inferFailsAssertion "GetMissingField" isExpectedError $
        getField (record KVal fields) getFieldTag
    | (name, getFieldTag, fields) <-
      [ ("GetField hole on empty record", hole, [])
      , ("GetField non-hole on empty record", tagStr "field", [])
      , ("GetField on one mismatching field", tagStr "bar", [(tagStr "foo", hole)])
      , ("GetField on one mismatching fields", tagStr "baz", [(tagStr "foo", hole), (tagStr "baz", hole)])
      ]
    ]
  , let
      isExpectedError (InferError Infer.Mismatch {}) = True
      isExpectedError _ = False
    in
      testGroup "must-be-the-one-field"
      [ testCase name .
        allowFailAssertion "GetField doesn't yet handle record-of-one" .
        inferFailsAssertion "Mismatch" isExpectedError $
        getDef "id" $$ integerType $$
        getField (record KVal [(fieldsTag, getDef "True")]) getFieldTag
      | (name, fieldsTag, getFieldTag) <-
        [ ("both tags are holes", hole, hole)
        , ("fields' tag is hole", hole, tagStr "foo")
        , ("getField tag is hole", tagStr "foo", hole)
        ]
      ]
  , testGroup "allowed getFields"
    [ testInfer "GetField hole of hole" $ getField hole (holeTag)
    , testInfer "GetField hole of record" $
      getField (record KVal [(holeTag, hole)]) holeTag
    , testInfer "GetField hole of record of 2" $
      getField (record KVal [(holeTag, hole), (holeTag, hole)]) holeTag
    , testInfer "GetField tag of record of 2" $
      getField (record KVal [(holeTag, hole), (holeTag, hole)]) (tagStr "foo")
    ]
  ]

hunitTests =
  simpleTests
  ++
  [ mapIdTest
  , testInfer "factorial" factorialExpr
  , testInfer "euler1" euler1Expr
  , testInferAllowFail "Missing type inferences"
    "solveDepressedQuartic" solveDepressedQuarticExpr
  , applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnARecord
  , idOnHole
  , inferFromOneArgToOther
  , inferFromOneArgToOtherMap
  , depApply
  , forceMono
  , idPreservesDependency
  , fOfXIsFOf5
  , monomorphRedex
  , inferPart
  , failResumptionAddsRules
  , emptyRecordTests
  , recordTest
  , inferReplicateOfReplicate
  -- , implicitVarTests
  , infiniteTypeTests
  , resumptionTests
  , joinMaybe
  , scopeEscape
  , tagCompositeTests
  , getFieldTests
  ]

inferPreservesShapeProp :: Expr -> Property
inferPreservesShapeProp expr =
  case runLoadInferDef expr of
    Left _ -> property rejected
    Right inferred -> property (void inferred == expr)

qcProps =
  [ testProperty "infer preserves shape" inferPreservesShapeProp
  ]

allTests = hunitTests ++ qcProps
