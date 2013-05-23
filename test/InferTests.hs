{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror #-}
module InferTests (allTests) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Exception (evaluate)
import Control.Lens.Operators
import Control.Monad (join, void)
import Control.Monad.Trans.State (runStateT, evalState, runState)
import Data.Monoid (Monoid(..))
import InferCombinators
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (inferWithConflicts)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)
import Test.QuickCheck (Property)
import Test.QuickCheck.Property (property, rejected)
import Utils
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Test.HUnit as HUnit


assertCompareInferred ::
  InferResults t -> InferResults t -> HUnit.Assertion
assertCompareInferred result expected =
  assertBool errMsg (Map.null resultErrs)
  where
    errMsg =
      List.intercalate "\n" $ show
      (resultExpr
       & ExprLens.exprDef %~ defIStr
       & Lens.mapped %~ ixsStr) :
      "Errors:" :
      (map showErrItem . Map.toList) resultErrs
    showErrItem (ix, err) = "{" ++ show ix ++ "}:\n" ++ err
    defIStr = BS8.unpack . BS8.takeWhile (/= '\0') . Guid.bs . IRef.guid
    ixsStr [] = UnescapedStr ""
    ixsStr ixs = UnescapedStr . List.intercalate ", " $ map show ixs
    (resultExpr, resultErrs) =
      runState (ExprUtil.matchExpression match mismatch result expected) Map.empty
    addError msg = do
      ix <- State.gets Map.size
      State.modify $ Map.insert ix msg
      pure ix
    check s x y
      | ExprUtil.alphaEq x y = pure []
      | otherwise = fmap (: []) . addError $
        List.intercalate "\n"
        [ "  expected " ++ s ++ ":" ++ show y
        , "  result   " ++ s ++ ":" ++ show x
        ]
    match (v0, t0) (v1, t1) =
      (++) <$> check " type" t0 t1 <*> check "value" v0 v1
    mismatch _ _ = error "Result must have same expression shape!"

testInfer ::
  String -> InferResults t -> HUnit.Test
testInfer name expr =
  testCase name $ assertCompareInferred inferredExpr expr
  where
    inferredExpr = inferResults . fst . doInfer_ $ void expr

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int" $
    leafSimple (Expr.LiteralInteger 5) intType
  , testInfer "simple apply" $
    iexpr pureHole pureHole $
    ExprUtil.makeApply
      (inferredHole (purePi "" pureHole pureHole))
      (inferredHole pureHole)
  , testInfer "simple pi" $
    iexpr
      (purePi "pi" pureHole pureHole)
      setType $
    namedPi ""
      (inferredHole setType)
      (inferredHole setType)
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply" $
  iexpr
    (pureApply [pureGetDef "IntToBoolFunc", pureHole])
    (pureGetDef "Bool") $
  ExprUtil.makeApply
    (getDef "IntToBoolFunc")
    (inferredHole intType)

inferPart :: HUnit.Test
inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  iexpr
    (exprWithHoles intType intType)
    endoListOfInt $
  namedLambda "xs"
    (iexpr (listOf intType) setType
      (ExprUtil.makeApply
        (getDef "List")
        (leaf Expr.Hole intType setType)
      )
    ) $
  iexpr
    (applyWithHole intType)
    (listOf intType) $
  ExprUtil.makeApply
    ( iexpr
        (pureApply [pureGetDef ":", intType, five])
        endoListOfInt
      ( ExprUtil.makeApply
        ( iexpr
            (pureApply [pureGetDef ":", intType])
            (purePi "" intType endoListOfInt)
          (ExprUtil.makeApply
            (getDef ":")
            (leaf Expr.Hole intType setType)
          )
        )
        (leafSimple (Expr.LiteralInteger 5) intType)
      )
    ) $
  getParam "xs" $ listOf intType
  where
    endoListOfInt = join (purePi "pi") $ listOf intType
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
  namedLambda "lambda" (inferredHole setType) $
  iexpr
    (pureApply [pureGetDef "IntToBoolFunc", pureHole])
    (pureGetDef "Bool") $
  ExprUtil.makeApply (getDef "IntToBoolFunc") $
  iexpr
    pureHole
    intType $
  ExprUtil.makeApply
    (inferredHole (purePi "" pureHole intType)) $
  leafSimple
    ((Expr.GetVariable . Expr.ParameterRef . Guid.fromString) "lambda")
    pureHole

idTest :: HUnit.Test
idTest =
  testInfer "id test" $
  iexpr
    (ExprLens.pureExpr # ExprUtil.makeApply (pureGetDef "id") intType)
    (purePi "" intType intType) $
  ExprUtil.makeApply
    (getDef "id") $
    leafSimple Expr.IntegerType setType

inferFromOneArgToOther :: HUnit.Test
inferFromOneArgToOther =
  testInfer "f = \\ a b (x:Map _ _) (y:Map a b) -> if {_ x y}" .
  iexpr expr (purePi "a" setType lamBType) $
  namedLambda "a"
    (leaf Expr.Hole setType setType) $
  iexpr lamB lamBType $
  namedLambda "b"
    (leaf Expr.Hole setType setType) $
  iexpr lamX lamXType $
  namedLambda "x"
    ( iexpr typeOfX setType $
      ExprUtil.makeApply
        ( iexpr typeOfX' setToSet $
          ExprUtil.makeApply
            (getDef "Map") $
          leaf Expr.Hole (pureGetParam "a") setType
        ) $
      leaf Expr.Hole (pureGetParam "b") setType
    ) $
  iexpr lamY lamYType $
  namedLambda "y"
    ( iexpr typeOfX setType $
      ExprUtil.makeApply
        ( iexpr typeOfX' setToSet $
          ExprUtil.makeApply
            (getDef "Map") $
          getParam "a" setType
        ) $
      getParam "b" setType
    ) $
  iexpr body typeOfX $
  ExprUtil.makeApply
    ( iexpr body1 body1Type $
      ExprUtil.makeApply
        (getDef "if") $
      leaf Expr.Hole typeOfX setType
    ) $
  iexpr ifParams ifParamsType $
  Expr.BodyRecord $ Expr.Record Val
  [ (leafTag t0, leafSimple Expr.Hole (pureGetDef "Bool"))
  , (leafTag t1, getParam "x" typeOfX)
  , (leafTag t2, getParam "y" typeOfX)
  ]
  where
    [t0, t1, t2] = defParamTags "if"
    expr = pureLambda "a" setType lamB
    lamB = pureLambda "b" setType lamX
    lamX = pureLambda "x" typeOfX lamY
    lamY = pureLambda "y" typeOfX body
    body = pureApply [body1, ifParams]
    body1 = pureApply [pureGetDef "if", typeOfX]
    ifParams =
      ExprUtil.pureExpression . Expr.BodyRecord $
      Expr.Record Val
      [ (tag t0, pureHole)
      , (tag t1, pureGetParam "x")
      , (tag t2, pureGetParam "y")
      ]
    ifParamsType =
      ExprUtil.pureExpression . Expr.BodyRecord $
      Expr.Record Type
      [ (tag t0, pureGetDef "Bool")
      , (tag t1, typeOfX)
      , (tag t2, typeOfX)
      ]
    tag = ExprUtil.pureExpression . Expr.BodyLeaf . Expr.Tag
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
    setToSet = purePi "setToSet" setType setType
    lamBType = purePi "b" setType lamXType
    lamXType = purePi "x" typeOfX lamYType
    lamYType = purePi "y" typeOfX typeOfX

monomorphRedex :: HUnit.Test
monomorphRedex =
  testInfer "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Set -> _ -> a) = _" .
  iexpr inferredExpr pureHole $
  ExprUtil.makeApply
    ( iexpr (bodyLambda True) bodyLambdaType $
      namedLambda "f"
        (leaf Expr.Hole fType setType) $
      iexpr (body True) pureHole $
      ExprUtil.makeApply
        (getParam "f" fType) $
      iexpr (fArg True) (fArgType True) $
      namedLambda "b"
        (leaf Expr.Hole setType setType) $
      iexpr xToX (fArgInnerType True) $
      namedLambda "x"
        (leaf Expr.Hole (pureGetParam "b") setType) $
      iexpr (pureGetParam "x") (pureGetParam "b") $
      ExprUtil.makeApply
        ( iexpr cToX (purePi "" pureHole (pureGetParam "b")) $
          namedLambda "c"
            (leafSimple Expr.Hole setType) $
          getParam "x" $ pureGetParam "b"
        ) $
      leafSimple Expr.Hole pureHole
    ) $
  iexpr (fExpr True) fType $
  namedLambda "fArg"
    ( iexpr (fArgType True) setType $
      namedPi "b"
        (leafSimple Expr.Set setType) $
      iexpr (fArgInnerType True) setType $
      namedPi "x"
        (leaf Expr.Hole (pureGetParam "b") setType) $
      getParam "b" setType
    ) $
  leafSimple Expr.Hole pureHole
  where
    inferredExpr = pureApply [fExpr True, fArg True]
    bodyLambda isInferred =
      pureLambda "f" (if isInferred then fType else pureHole) $
      body isInferred
    fExpr isInferred = pureLambda "" (fArgType isInferred) pureHole
    fArgType isInferred = purePi "b" setType $ fArgInnerType isInferred
    fArgInnerType False = purePi "" pureHole $ pureGetParam "b"
    fArgInnerType True = purePi "" (pureGetParam "b") $ pureGetParam "b"
    body isInferred = pureApply [pureGetParam "f", fArg isInferred]
    fArg False =
      pureLambda "b" pureHole .
      pureLambda "x" pureHole $
      pureApply [cToX, pureHole]
    fArg True = pureLambda "b" setType xToX
    cToX = pureLambda "c" pureHole $ pureGetParam "x"
    xToX = pureLambda "x" (pureGetParam "b") $ pureGetParam "x"
    bodyLambdaType = purePi "" fType pureHole
    fType = purePi "" (fArgType True) pureHole

fOfXIsFOf5 :: HUnit.Test
fOfXIsFOf5 =
  testInfer "f x = f 5" $
  iexpr
    (pureLambda "" intType (pureApply [getRecursiveDef, five]))
    (purePi "" intType pureHole) $
  namedLambda "x"
    (leaf Expr.Hole intType setType) $
  iexpr
    (pureApply [getRecursiveDef, five])
    pureHole $
  ExprUtil.makeApply
    (leafSimple
      (Expr.GetVariable (Expr.DefinitionRef recursiveDefI))
      (purePi "" intType pureHole)) $
  leafSimple (Expr.LiteralInteger 5) intType

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi" $
  iexpr
    pureHole
    pureHole $
  ExprUtil.makeApply
    (inferredHole (purePi "" intType pureHole)) $
  leafSimple (Expr.LiteralInteger 5) intType

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int" $
  iexpr
    (pureApply
      [ pureGetDef "id"
      , intType
      , five
      ]
    )
    intType $
  ExprUtil.makeApply
    (iexpr
      (pureApply [pureGetDef "id", intType])
      (purePi "" intType intType)
      (ExprUtil.makeApply
        (getDef "id")
        ((iexpr intType setType . Expr.BodyLeaf) Expr.Hole)
      )
    ) $
  leafSimple (Expr.LiteralInteger 5) intType

idOnHole :: HUnit.Test
idOnHole =
  testInfer "id hole" .
  iexpr
    (pureApply [pureGetDef "id", pureHole])
    (purePi "" pureHole pureHole) .
  ExprUtil.makeApply
    (getDef "id") $
  inferredHole setType

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
    setType $
  ExprUtil.makeApply
    (iexpr
      idSet
      (purePi "" setType setType)
      (ExprUtil.makeApply
        (getDef "id")
        (leaf Expr.Hole setType setType)
      )
    ) $
  leafSimple Expr.Hole setType
  where
    idSet = pureApply [pureGetDef "id", setType]
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
    inferredT = getParam "t" setType
    inferredRT = getParam "rt" rtParamType
    inferredRTType =
      iexpr rtParamType setType $
      namedPi "" inferredT inferredSetType
    inferredFParamType =
      iexpr fParamType setType . namedPi "d" inferredT .
      iexpr (rtAppliedTo "d") setType .
      ExprUtil.makeApply inferredRT .
      getParam "d" $
      pureGetParam "t"
    inferredXParamType = inferredT
    lamPi name paramType (body, bodyType) =
      ( pureLambda name paramType body
      , purePi name paramType bodyType
      )
    (tLambda, tPi) = lamPi "t" setType rt
    rt@(rtLambda, rtPi) = lamPi "rt" rtParamType f
    f@(fLambda, fPi) = lamPi "f" fParamType x
    x@(xLambda, xPi) = lamPi "x" xParamType (fOfX, rtAppliedTo "x")
    fOfX = ExprUtil.pureExpression . ExprUtil.makeApply (pureGetParam "f") $ pureGetParam "x"
    rtParamType = purePi "" (pureGetParam "t") setType
    fParamType =
      purePi "d" (pureGetParam "t") $ rtAppliedTo "d"
    xParamType = pureGetParam "t"
    rtAppliedTo name =
      ExprUtil.pureExpression . ExprUtil.makeApply (pureGetParam "rt") $ pureGetParam name

testCase :: String -> HUnit.Assertion -> HUnit.Test
testCase name = HUnit.TestLabel name . HUnit.TestCase

testResume ::
  String -> PureExprDefI t ->
  PureExprDefI t ->
  Lens.Traversal'
    (Expression (DefI t) (Infer.Inferred (DefI t)))
    (Expression (DefI t) (Infer.Inferred (DefI t))) ->
  HUnit.Test
testResume name newExpr testExpr lens =
  testCase name $
  let
    (tExpr, inferContext) = doInfer_ testExpr
    Just pl = tExpr ^? lens . Expr.ePayload
  in
    void . evaluate . (`runStateT` inferContext) $
    doInferM (Infer.iPoint pl) newExpr

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
      pureLambda "b" setType .
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
      leafSimple (Expr.GetVariable (Expr.DefinitionRef recursiveDefI)) $
      purePi "" pureHole pureHole
  ]
  where
    lamBody :: Lens.Traversal' (Expression def a) (Expression def a)
    lamBody = ExprLens.exprLam . Expr.lambdaResult

parRef :: String -> PureExpr def
parRef =
  ExprUtil.pureExpression . Lens.review ExprLens.bodyParameterRef . Guid.fromString

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
      pureApply [parRef "x", pureHole, pureHole]

emptyRecordTest :: HUnit.Test
emptyRecordTest =
  testInfer "empty record type infer" $
  iexpr emptyRecordType setType emptyRecordTypeBody
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
  namedLambda "a" (leafSimple Expr.Set setType) $
  iexpr lamX piX $
  namedLambda "x" (getParam "a" setType) $
  iexpr recVal recType $
  Expr.BodyRecord $
  Expr.Record Val
  [( leafSimple fieldTagLeaf tagType
   , getParam "x" (pureGetParam "a")
   )]
  where
    lamA =
      pureLambda "a" setType lamX
    lamX =
      pureLambda "x" (pureGetParam "a") recVal
    recVal =
      ExprUtil.pureExpression $
      rec Val $
      pureGetParam "x"
    fieldTagLeaf = Expr.Tag fieldGuid
    rec k =
      Expr.BodyRecord . Expr.Record k .
      (:[]) . (,) (ExprUtil.pureExpression (Expr.BodyLeaf fieldTagLeaf))
    fieldGuid = Guid.fromString "field"
    piA =
      purePi "a" setType piX
    piX =
      purePi "x" (pureGetParam "a") recType
    recType =
      ExprUtil.pureExpression $
      rec Type $
      pureGetParam "a"

inferRecordValTest :: HUnit.Test
inferRecordValTest =
  testInfer "id ({:Set) <hole> infers { val" $
  makeApply [getDef "id", record Type [], inferredVal (record Val [])]

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
