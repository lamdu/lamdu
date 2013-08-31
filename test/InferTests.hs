{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module InferTests (allTests, factorialExpr, euler1Expr, solveDepressedQuarticExpr) where

import Control.Lens.Operators
import Control.Monad (void)
import InferAssert
import InferCombinators
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Kind(..), Expression(..))
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
  testInfer "foo = f (\\b (x:{b}) -> (\\_1 -> x) ?) where f (_2:(a:Type -> ? -> a)) = ?" $
  whereItem "f" (lambda "_2" fArgType (const hole)) $ \f ->
  f $$
  lambda "b" (asHole set)
  (\b ->
   lambda "x" (asHole b) $ \x ->
   lambda "_1" (holeWithInferredType set) (const x) $$ hole)
  where
    -- (a:Type -> _[=a] -> a)
    fArgType = piType "a" set $ \a -> asHole a ~> a

idPreservesDependency =
  testInfer "5 + f _ where f x = id _{no inferred type}" $
  whereItem "f" (lambda "x" iset (const (getDef "id" $$ iset $$ hole))) $ \f ->
  getDef "+" $$ asHole integerType $$:
  [literalInteger 5, (f $$ hole) `setInferredType` integerType]
  where
    iset = holeWithInferredType set

idTest = testInfer "id test" $ getDef "id" $$ integerType

inferFromOneArgToOther =
  testInfer "f = \\ (a:set) (b:a) (c:a) -> if<a> {True b c}" $
  lambda "a" set $ \a ->
  lambda "b" a $ \b ->
  lambda "c" a $ \c ->
  getDef "if" $$ a $$:
  [getDef "True", b, c]

inferFromOneArgToOtherList =
  testInfer "f = \\ a (b:List _) (c:List a) -> if {_ b c}" $
  lambda "a" (asHole set) $ \a ->
  lambda "b" (listOf (asHole a)) $ \b ->
  lambda "c" (listOf a) $ \c ->
  getDef "if" $$ asHole (listOf a) $$:
  [holeWithInferredType (getDef "Bool"), b, c]

inferFromOneArgToOtherMap =
  testInfer "f = \\ a b (c:Map _ _) (d:Map a b) -> if {_ c d}" $
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

idType :: InputExpr
idType = piType "a" set $ \a -> a ~> a

failResumptionAddsRules =
  -- f     x    = x _ _
  --   --------
  --   (_ -> _)
  --         ^ Set Bool here
  testCase "Resumption that adds rules and fails" .
  runContextAssertion $ do
    -- TODO: Use standard resumption APIs for failed resumes too?
    rootInferred <- inferDef $ infer =<< load expr
    fmap verifyError . try . void $
      loadInferInto (rootInferred ^?! resumptionPoint) resumptionValue
  where
    verifyError :: Either Error () -> M ()
    verifyError (Left (InferError Infer.Mismatch {})) = return ()
    verifyError _ = error "Resumption did not fail!"
    expr = lambda "x" (hole ~> hole) $ \x -> x $$ hole $$ hole
    resumptionValue = getDef "Bool" -- <- anything but Pi
    resumptionPoint =
      lamParamType KVal . lamResult KType .
      Expr.ePayload . Lens._1

testRecurseResumption =
  testInfer "Resumption with recursion" $
  lambda "a" (asHole oldFuncType `resumeHere` newFuncType) $ \a ->
  a $$ (recurse (((hole ~> hole) `resumedTo` newFuncType) ~> (hole `resumedTo` integerType)) $$ a)
  where
    oldFuncType = holeWithInferredType set ~> holeWithInferredType set
    newFuncType = asHole integerType ~> integerType

resumptionTests =
  testGroup "type infer resume" $
  [ testInfer "{hole->pi}" $
    hole `resumeHere` (holeWithInferredType set ~> holeWithInferredType set)
  , testInfer "{hole->id} hole" $
    holeWithInferredType (hole ~> hole)
    `resumedTo`
    holeWithInferredType (idType ~> hole) $$
    (hole `resumeHere` getDef "id")
  , testInfer "\\_:hole -> {hole->id}" $
    lambda "" (holeWithInferredType set) $ \_ -> hole `resumeHere` getDef "id"
  , failResumptionAddsRules
  , testRecurseResumption
  , testInfer "Resumption with list" $
    list [hole `resumeHere` literalInteger 1]
  , testInfer "Resumption with getfield" $
    getField
    (hole `resumeHere` record KVal [(tagStr "x", literalInteger 5)])
    (tagStr "x")
  , testInfer "apply of resumed-get-def" $
    holeWithInferredType (getDef "Bool" ~> hole) `resumeHere` getDef "not" $$
    getDef "True"
  , testInfer "apply of resumed-lam" $
    holeWithInferredType (hole ~> hole)
    `resumeHere` lambda "x" (holeWithInferredType set) (const hole)
    $$ hole
  ] ++
  let
    lambdaABTest varName sel =
      testInfer ("\\a:hole -> \\b:hole -> {hole->" ++ varName ++ "}") $
      lambda "a" (holeWithInferredType set) $ \a ->
      lambda "b" (holeWithInferredType set) $ \b ->
      hole `resumeHere` sel (a, b)
  in [lambdaABTest "a" fst, lambdaABTest "b" snd] ++
  [ testInfer "\\a:hole -> \\b:set -> \\c:hole -> c a {hole->b}" $
    lambda "a" (holeWithInferredType set) $ \a ->
    lambda "b" set $ \b ->
    -- TODO: With rigidity tests, the inferred type of "c" is going to
    -- be (hole~>hole~>hole)
    lambda "c" (asHole (hole ~> hole)) $ \c ->
    (c $$ a)
    `setInferredType` (hole ~> hole)
    `resumedToType` (set ~> hole) $$
    hole `resumeHere` b
  , testInfer "ref to the def on the side" $
    lambda "" (holeWithInferredType set) $ \_ ->
    hole `resumeOnSide` recurse (hole ~> hole)
  ]

lamParamType :: Kind -> Lens.Traversal' (Expression def a) (Expression def a)
lamParamType k = ExprLens.exprKindedLam k . Lens._2
lamResult :: Kind -> Lens.Traversal' (Expression def a) (Expression def a)
lamResult k = ExprLens.exprKindedLam k . Lens._3

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

simplestImplicitTest =
  testCase "simplestImplicitTest: \\(x:?) -> ?" $
  inferWVAssertion (expr (holeWithInferredType set)) wvExpr
  where
    expr a = lambda "x" (asHole a) $ \_ -> hole
    wvExpr = lambda "a" (asHole set) expr

-- TODO: Test should verify that ImplicitVariables puts the restricted
-- form (b->c) in the right place.
addImplicitCurriedApply2Test =
  testCase "implicitCurriedApply2: \\f -> f ? ?" $
  allowFailAssertion "Missing rigidity check" $
  inferWVAssertion (expr hole) wvExpr
  where
    expr a =
      lambda "f" (asHole (a ~> hole)) $ \f ->
      (f $$ holeWithInferredType a) `setInferredType` (hole ~> hole) $$ hole
    wvExpr = lambda "a" (asHole set) expr

uncurry2Test =
  testCase "uncurry2: \\params:{?->?->?, x:?, y:?} -> f x y   WV: \\a b c params:{f:?{a->b->c} x:?a y:?b} -> f x y : c" $
  allowFailAssertion "Missing rigidity check" $
  inferWVAssertion (expr iset iset iset) wvExpr
  where
    iset = holeWithInferredType set
    expr a b c =
      lambdaRecord "params"
      [ ("f", asHole (b ~> a ~> c))
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
  [ simplestImplicitTest
  , addImplicitCurriedApply2Test
  , uncurry2Test
  ]

implicitLamReturningRecord =
  testCase "add implicit structure" $
  inferWSAssertion
  (expr (holeWithInferredType (recType ~> recType)))
  (expr (asHole . lambda "x" recType . const $ mkRecord KVal unknownInt unknownInt))
  where
    unknownInt = holeWithInferredType integerType
    expr next = getDef "iterate" $$ asHole recType $$: [initial, next]
    initial = mkRecord KVal (literalInteger 0) (literalInteger 1)
    recType = mkRecord KType integerType integerType
    mkRecord k cur next =
      record k
      [ (tagStr "cur", cur)
      , (tagStr "next", next)
      ]

implicitStructureTests =
  testGroup "implicit structure"
  [ implicitLamReturningRecord
  ]

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

fix3Lambdas =
  testInfer "fix3Lambdas: fix (\\a -> \\b -> \\c -> ?)" $
  getDef "fix" $$ asHole (hole ~> hole ~> hole) $$
  ( l "a" (hole ~> hole ~> hole) $
    l "b" (holeWithInferredType set) $
    l "c" (holeWithInferredType set) $
    hole
  )
  where
    l n t = lambda n (asHole t) . const

infiniteTypeTests =
  testGroup "Infinite types"
  [ wrongRecurseMissingArg
  , getFieldWasntAllowed
  , fix3Lambdas
  ]

getFieldWasntAllowed =
  testInfer "map (\\x:_. #x#) {}:_" $
  getDef "map" $$ asHole recType $$ holeWithInferredType set `resumedTo` asHole recType $$:
  [ getDef ":" $$ asHole recType $$:
    [ recVal
    , holeWithInferredType $ listOf recType
    ]
  , lambda "x" (asHole recType) $ \x ->
    hole `resumeHere` x
  ]
  where
    recVal = record KVal []
    recType = record KType []

wrongRecurseMissingArg =
  testCase "f x = f" .
  inferFailsAssertion "InfiniteExpression" isExpectedError $
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
  [ ("e", iInt)
  , ("d", iInt)
  , ("c", iInt)
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
      , ("get field in tag of get field", getField hole (getField hole hole))
      ]

getFieldTests =
  testGroup "GetField tests"
  [ testGroup "missing fields"
    [ let
        isExpectedError (InferError Infer.GetMissingField {}) = True
        isExpectedError
          (InferError
           (Infer.Mismatch
            (Expr.BodyLeaf Expr.Tag{})
            (Expr.BodyLeaf Expr.Tag{}))) = True
        isExpectedError _ = False
      in
        testCase name .
        inferFailsAssertion "GetMissingField" isExpectedError $
        getField (record KVal fields) getFieldTag
    | (name, getFieldTag, fields) <-
      [ ("GetField hole on empty record", hole, [])
      , ("GetField non-hole on empty record", tagStr "field", [])
      , ("GetField on one mismatching field", tagStr "bar", [(tagStr "foo", hole)])
      , ("GetField on two mismatching fields", tagStr "bar", [(tagStr "foo", hole), (tagStr "baz", hole)])
      ]
    ]
  , let
      isExpectedError (InferError Infer.Mismatch {}) = True
      isExpectedError _ = False
    in
      testGroup "must-be-the-one-field"
      [ testCase name .
        inferFailsAssertion "Mismatch" isExpectedError $
        getDef "id" $$ integerType $$
        getField (record KVal [(fieldsTag, getDef "True")]) getFieldTag
      | (name, fieldsTag, getFieldTag) <-
        [ ("both tags are holes", hole, hole)
        , ("fields' tag is hole", hole, tagStr "foo")
        , ("getField tag is hole", tagStr "foo", hole)
        ]
      ]
  , let
      isExpectedError (InferError Infer.GetFieldRequiresRecord {}) = True
      isExpectedError _ = False
    in
      testCase "getField of non-record" $
      inferFailsAssertion "GetFieldRequiresRecord" isExpectedError $
      getField integerType holeTag
  , testGroup "allowed getFields"
    [ testInfer "GetField hole of hole" $ getField hole holeTag
    , testInfer "GetField hole of record" $
      getField (record KVal [(holeTag, hole)]) holeTag
    , testInfer "GetField hole of record of 2" $
      getField (record KVal [(holeTag, hole), (holeTag, hole)]) holeTag
    , testInfer "GetField tag of record of 2" $
      getField (record KVal [(holeTag, hole), (holeTag, hole)]) (tagStr "foo")
    ]
  , let intGetField r t = typeAnnotate integerType $ getField r t
    in
    testGroup "resumed tags"
    [ testInfer "GetField (resumed tag) verified against record tags" $
      intGetField
      ( record KVal
        [ (tagStr "x", hole `resumedToType` integerType)
        , (tagStr "y", hole)
        ]
      ) (holeTag `resumeHere` tagStr "x")
    , testInfer "GetField verified against resumed (record tags)" $
      intGetField
      ( record KVal
        [ (holeTag `resumeHere` tagStr "x", hole)
        , (holeTag `resumedTo` tagStr "y", hole `resumedToType` integerType)
        ]
      ) (tagStr "y")
    , testInfer "GetField verified against (resumed record) tags" $
      intGetField
      ( hole
        `resumeHere`
        record KVal
        [ (tagStr "x", literalInteger 5)
        , (tagStr "y", hole)
        ]
      ) (tagStr "x")
    ]
  ]

fromQuickCheck1 =
  testCase "fromQuickCheck1: \\a -> a (a a)" .
  inferFailsAssertion "GetFieldRequiresRecord" isExpectedError $
  -- QuickCheck discovered: ? (\\a:?==>a (25 (a (a (a a)) ?)))
  -- simplified to:
  lambda "a" hole $ \a -> a $$ (a $$ a)
  where
    isExpectedError (InferError Infer.InfiniteExpression {}) = True
    isExpectedError _ = False

testUnificationCarriesOver =
  testGroup "Unification carries over"
  [ testInfer "(\\(a:Set) -> (+) _) _ :: ({l:IntegerType, r:_}->_)" $
    typeAnnotate
    (recType integerType (asHole integerType) ~> asHole integerType) $
    lambda "a" set (\_ -> getDef "+" $$ holeWithInferredType set) $$
    holeWithInferredType set

  , testInfer "(\\(a:Set) -> ?{(+)} ?) ? :: ({l:IntegerType, r:?}->?)" $
    typeAnnotate
    (recType integerType (holeWithInferredType set `resumedTo` integerType)
     ~> (holeWithInferredType set `resumedTo` integerType)) $
    lambda "a" set
    ( \_ ->
      (holeWithInferredType (hole ~> hole) `resumeHere` getDef "+") $$ hole `resumedToType` set
    ) $$ holeWithInferredType set

  , testInfer
    "(\\(_1:Set) -> (? :: (a:?{Type}) -> {l:?{a}, r:?{a}} -> ?{a}) ?) ? :: {l:IntegerType, r:?} -> ?" $
    typeAnnotate
    (recType integerType (holeWithInferredType set `resumedTo` integerType)
     ~> (holeWithInferredType set `resumedTo` integerType)) $
    lambda "_1" set
    ( \_ ->
      typeAnnotate
      ( piType "a" set
        (\a ->
          recType
          (holeWithInferredType set `resumeHere` a)
          (holeWithInferredType set `resumeHere` a) ~>
          (holeWithInferredType set `resumeHere` a))
      )
      hole $$
      holeWithInferredType set
    ) $$ holeWithInferredType set
  ]
  where
    recType lType rType = record KType [(tagStr "infixlarg", lType), (tagStr "infixrarg", rType)]

testUnifiedDependentPis =
  testInfer "if _ (_ :: a:Set -> a -> _) (_ :: b:Set -> _ -> b)" $
  getDef "if" $$ asHole (theType "ifvar" id id) $$:
  [ holeWithInferredType (getDef "Bool")
  , typeAnnotate (theType "a" id asHole) hole
  , typeAnnotate (theType "b" asHole id) hole
  ]
  where
    theType name onFst onSnd =
      piType name set $ \t ->
      onFst t ~> onSnd t

hunitTests =
  simpleTests
  ++
  [ fromQuickCheck1
  , mapIdTest
  , testInfer "factorial" factorialExpr
  , testInfer "euler1" euler1Expr
  , testInfer "solveDepressedQuartic" solveDepressedQuarticExpr
  , applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnARecord
  , idOnHole
  , inferFromOneArgToOther
  , inferFromOneArgToOtherList
  , inferFromOneArgToOtherMap
  , depApply
  , forceMono
  , idPreservesDependency
  , fOfXIsFOf5
  , monomorphRedex
  , inferPart
  , emptyRecordTests
  , recordTest
  , inferReplicateOfReplicate
  , implicitVarTests
  , implicitStructureTests
  , infiniteTypeTests
  , resumptionTests
  , joinMaybe
  , scopeEscape
  , tagCompositeTests
  , getFieldTests
  , testUnificationCarriesOver
  , testUnifiedDependentPis
  ]

inferPreservesShapeProp :: Expr () -> Property
inferPreservesShapeProp expr =
  case runLoadInferDerefDef expr of
    Left _ -> property rejected
    Right inferred -> property (void inferred == expr)

qcProps =
  [ testProperty "infer preserves shape" inferPreservesShapeProp
  ]

allTests = hunitTests ++ qcProps
