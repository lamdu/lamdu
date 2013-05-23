{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall -Werror #-}
module InferTests (allTests) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Exception (evaluate)
import Control.Lens.Operators
import Control.Monad (join, void)
import Control.Monad.Trans.State (runStateT, evalState)
import Data.Map ((!))
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..))
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
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Test.HUnit as HUnit

type InferResults def =
  Expression.Expression def
  ( PureExpr def
  , PureExpr def
  )

mkInferredGetDef :: String -> InferResults (DefI t)
mkInferredGetDef name =
  mkInferredLeafSimple
  (Expression.GetVariable . Expression.DefinitionRef $ IRef.unsafeFromGuid g)
  (void (definitionTypes ! g))
  where
    g = Guid.fromString name

mkInferredLeafTag :: Guid -> InferResults (DefI t)
mkInferredLeafTag guid =
  mkInferredLeafSimple (Expression.Tag guid) .
  ExprUtil.pureExpression $ Expression.BodyLeaf Expression.TagType

defParamTags :: String -> [Guid]
defParamTags defName =
  innerMostPi (definitionTypes ! g) ^..
  Expression.eBody . Expression._BodyLam . Expression.lambdaParamType .
  Expression.eBody . Expression._BodyRecord .
  Expression.recordFields . Lens.traversed . Lens._1 .
  Expression.eBody . Expression._BodyLeaf . Expression._Tag
  where
    g = Guid.fromString defName

innerMostPi :: Expression def a -> Expression def a
innerMostPi =
  last . pis
  where
    pis expr =
      case expr ^? Expression.eBody . Expression._BodyLam of
      Just lam
        | lam ^. Expression.lambdaKind == Expression.Type ->
          expr : pis (lam ^. Expression.lambdaResult)
      _ -> []

mkInferredGetParam :: String -> PureExpr def -> InferResults def
mkInferredGetParam name = mkInferredLeafSimple gv
  where
    gv = Expression.GetVariable . Expression.ParameterRef $ Guid.fromString name

inferResults :: DataIRef.Expression t (Infer.Inferred (DefI t)) -> InferResults (DefI t)
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
        expr = inferredExpr ^. Expression.eBody
        (val, typ) = inferredExpr ^. Expression.ePayload

compareInferred :: InferResults (DefI t) -> InferResults (DefI t) -> Bool
compareInferred x y =
  isJust $ ExprUtil.matchExpression f ((const . const) Nothing) x y
  where
    f (v0, t0) (v1, t1) =
      liftA2 (,) (matchI v0 v1) (matchI t0 t1)
    matchI = ExprUtil.matchExpression nop ((const . const) Nothing)
    nop () () = Just ()

mkInferredLeafSimple :: Expression.Leaf def -> PureExpr def -> InferResults def
mkInferredLeafSimple leaf =
  mkInferredLeaf leaf . ExprUtil.pureExpression $ Expression.BodyLeaf leaf

mkInferredLeaf ::
  Expression.Leaf def -> PureExpr def -> PureExpr def ->
  InferResults def
mkInferredLeaf leaf val typ =
  mkInferredNode val typ $ Expression.BodyLeaf leaf

mkInferredNode ::
  PureExpr def ->
  PureExpr def ->
  Expression.Body def (InferResults def) -> InferResults def
mkInferredNode iVal iType body =
  Expression.Expression body (iVal, iType)

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int"
    (ExprUtil.pureExpression (Lens.review ExprUtil.bodyLiteralInteger 5)) $
    mkInferredLeafSimple (Expression.LiteralInteger 5) intType
  , testInfer "simple apply"
    (pureApply [pureHole, pureHole]) $
    mkInferredNode pureHole pureHole $
    ExprUtil.makeApply
      (inferredHole (purePi "" pureHole pureHole))
      (inferredHole pureHole)
  , testInfer "simple pi"
    (purePi "pi" pureHole pureHole) $
    mkInferredNode
      (purePi "pi" pureHole pureHole)
      setType $
    makeNamedPi ""
      (inferredHole setType)
      (inferredHole setType)
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply"
  (pureApply [pureGetDef "IntToBoolFunc", pureHole]) $
  mkInferredNode
    (pureApply [pureGetDef "IntToBoolFunc", pureHole])
    (pureGetDef "Bool") $
  ExprUtil.makeApply
    (mkInferredGetDef "IntToBoolFunc")
    (inferredHole intType)

inferPart :: HUnit.Test
inferPart =
  testInfer "foo (xs:List ?) = 5 : xs"
  (exprWithHoles pureHole pureHole) $
  mkInferredNode
    (exprWithHoles intType intType)
    endoListOfInt $
  makeNamedLambda "xs"
    (mkInferredNode (listOf intType) setType
      (ExprUtil.makeApply
        (mkInferredGetDef "List")
        (mkInferredLeaf Expression.Hole intType setType)
      )
    ) $
  mkInferredNode
    (applyWithHole intType)
    (listOf intType) $
  ExprUtil.makeApply
    ( mkInferredNode
        (pureApply [pureGetDef ":", intType, five])
        endoListOfInt
      ( ExprUtil.makeApply
        ( mkInferredNode
            (pureApply [pureGetDef ":", intType])
            (purePi "" intType endoListOfInt)
          (ExprUtil.makeApply
            (mkInferredGetDef ":")
            (mkInferredLeaf Expression.Hole intType setType)
          )
        )
        (mkInferredLeafSimple (Expression.LiteralInteger 5) intType)
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
    (pureApply [ pureHole, pureGetParam "lambda" ] )
  ) $
  mkInferredNode
    (makeFunnyLambda "" pureHole)
    (purePi "" pureHole (pureGetDef "Bool")) $
  makeNamedLambda "lambda" (inferredHole setType) $
  mkInferredNode
    (pureApply [pureGetDef "IntToBoolFunc", pureHole])
    (pureGetDef "Bool") $
  ExprUtil.makeApply (mkInferredGetDef "IntToBoolFunc") $
  mkInferredNode
    pureHole
    intType $
  ExprUtil.makeApply
    (inferredHole (purePi "" pureHole intType)) $
  mkInferredLeafSimple
    ((Expression.GetVariable . Expression.ParameterRef . Guid.fromString) "lambda")
    pureHole

idTest :: HUnit.Test
idTest =
  testInfer "id test"
  applyIdInt $
  mkInferredNode
    applyIdInt
    (purePi "" intType intType) $
  ExprUtil.makeApply
    (mkInferredGetDef "id") $
    mkInferredLeafSimple Expression.IntegerType setType

inferFromOneArgToOther :: HUnit.Test
inferFromOneArgToOther =
  testInfer "f = \\ a b (x:Map _ _) (y:Map a b) -> if {_ x y}"
  (expr False) .
  mkInferredNode (expr True) (purePi "a" setType lamBType) $
  makeNamedLambda "a"
    (mkInferredLeaf Expression.Hole setType setType) $
  mkInferredNode (lamB True) lamBType $
  makeNamedLambda "b"
    (mkInferredLeaf Expression.Hole setType setType) $
  mkInferredNode (lamX True) lamXType $
  makeNamedLambda "x"
    ( mkInferredNode (typeOfX True) setType $
      ExprUtil.makeApply
        ( mkInferredNode (typeOfX' True) setToSet $
          ExprUtil.makeApply
            (mkInferredGetDef "Map") $
          mkInferredLeaf Expression.Hole (pureGetParam "a") setType
        ) $
      mkInferredLeaf Expression.Hole (pureGetParam "b") setType
    ) $
  mkInferredNode (lamY True) lamYType $
  makeNamedLambda "y"
    ( mkInferredNode (typeOfX True) setType $
      ExprUtil.makeApply
        ( mkInferredNode (typeOfX' True) setToSet $
          ExprUtil.makeApply
            (mkInferredGetDef "Map") $
          mkInferredGetParam "a" setType
        ) $
      mkInferredGetParam "b" setType
    ) $
  mkInferredNode (body True) (typeOfX True) $
  ExprUtil.makeApply
    ( mkInferredNode (body1 True) body1Type $
      ExprUtil.makeApply
        (mkInferredGetDef "if") $
      mkInferredLeaf Expression.Hole (typeOfX True) setType
    ) $
  mkInferredNode ifParams ifParamsType $
  Expression.BodyRecord $ Expression.Record Expression.Val
  [ (mkInferredLeafTag t0, mkInferredLeafSimple Expression.Hole (pureGetDef "Bool"))
  , (mkInferredLeafTag t1, mkInferredGetParam "x" (typeOfX True))
  , (mkInferredLeafTag t2, mkInferredGetParam "y" (typeOfX True))
  ]
  where
    [t0, t1, t2] = defParamTags "if"
    expr isInferred =
      pureLambda "a" (holeOr setType isInferred) $ lamB isInferred
    lamB isInferred =
      pureLambda "b" (holeOr setType isInferred) $ lamX isInferred
    lamX isInferred =
      pureLambda "x" (typeOfX isInferred) $ lamY isInferred
    lamY isInferred =
      pureLambda "y" (typeOfX True) $ body isInferred
    body isInferred = pureApply [body1 isInferred, ifParams]
    body1 isInferred =
      pureApply [pureGetDef "if", holeOr (typeOfX True) isInferred]
    ifParams =
      ExprUtil.pureExpression . Expression.BodyRecord $
      Expression.Record Expression.Val
      [ (tag t0, pureHole)
      , (tag t1, pureGetParam "x")
      , (tag t2, pureGetParam "y")
      ]
    ifParamsType =
      ExprUtil.pureExpression . Expression.BodyRecord $
      Expression.Record Expression.Type
      [ (tag t0, pureGetDef "Bool")
      , (tag t1, typeOfX True)
      , (tag t2, typeOfX True)
      ]
    tag = ExprUtil.pureExpression . Expression.BodyLeaf . Expression.Tag
    body1Type = purePi "body1" ifParamsType (typeOfX True)
    typeOfX isInferred =
      pureApply
      [ typeOfX' isInferred
      , holeOr (pureGetParam "b") isInferred
      ]
    typeOfX' isInferred =
      pureApply
      [ pureGetDef "Map"
      , holeOr (pureGetParam "a") isInferred
      ]
    setToSet = purePi "setToSet" setType setType
    holeOr _ False = pureHole
    holeOr x True = x
    lamBType = purePi "b" setType lamXType
    lamXType = purePi "x" (typeOfX True) lamYType
    lamYType = purePi "y" (typeOfX True) (typeOfX True)

monomorphRedex :: HUnit.Test
monomorphRedex =
  testInfer "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Set -> _ -> a) = _"
  expr .
  mkInferredNode inferredExpr pureHole $
  ExprUtil.makeApply
    ( mkInferredNode (bodyLambda True) bodyLambdaType $
      makeNamedLambda "f"
        (mkInferredLeaf Expression.Hole fType setType) $
      mkInferredNode (body True) pureHole $
      ExprUtil.makeApply
        (mkInferredGetParam "f" fType) $
      mkInferredNode (fArg True) (fArgType True) $
      makeNamedLambda "b"
        (mkInferredLeaf Expression.Hole setType setType) $
      mkInferredNode xToX (fArgInnerType True) $
      makeNamedLambda "x"
        (mkInferredLeaf Expression.Hole (pureGetParam "b") setType) $
      mkInferredNode (pureGetParam "x") (pureGetParam "b") $
      ExprUtil.makeApply
        ( mkInferredNode cToX (purePi "" pureHole (pureGetParam "b")) $
          makeNamedLambda "c"
            (mkInferredLeafSimple Expression.Hole setType) $
          mkInferredGetParam "x" $ pureGetParam "b"
        ) $
      mkInferredLeafSimple Expression.Hole pureHole
    ) $
  mkInferredNode (fExpr True) fType $
  makeNamedLambda "fArg"
    ( mkInferredNode (fArgType True) setType $
      makeNamedPi "b"
        (mkInferredLeafSimple Expression.Set setType) $
      mkInferredNode (fArgInnerType True) setType $
      makeNamedPi "x"
        (mkInferredLeaf Expression.Hole (pureGetParam "b") setType) $
      mkInferredGetParam "b" setType
    ) $
  mkInferredLeafSimple Expression.Hole pureHole
  where
    expr = pureApply [bodyLambda False, fExpr False]
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
  testInfer "f x = f 5"
  (pureLambda "x" pureHole (pureApply [getRecursiveDef, five])) $
  mkInferredNode
    (pureLambda "" intType (pureApply [getRecursiveDef, five]))
    (purePi "" intType pureHole) $
  makeNamedLambda "x"
    (mkInferredLeaf Expression.Hole intType setType) $
  mkInferredNode
    (pureApply [getRecursiveDef, five])
    pureHole $
  ExprUtil.makeApply
    (mkInferredLeafSimple
      (Expression.GetVariable (Expression.DefinitionRef recursiveDefI))
      (purePi "" intType pureHole)) $
  mkInferredLeafSimple (Expression.LiteralInteger 5) intType

five :: PureExprDefI t
five = ExprUtil.pureExpression $ Lens.review ExprUtil.bodyLiteralInteger 5

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi"
  (pureApply [pureHole, five]) $
  mkInferredNode
    pureHole
    pureHole $
  ExprUtil.makeApply
    (inferredHole (purePi "" intType pureHole)) $
  mkInferredLeafSimple (Expression.LiteralInteger 5) intType

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int"
  (pureApply
    [ pureGetDef "id"
    , pureHole
    , five
    ]
  ) $
  mkInferredNode
    (pureApply
      [ pureGetDef "id"
      , intType
      , five
      ]
    )
    intType $
  ExprUtil.makeApply
    (mkInferredNode
      (pureApply [pureGetDef "id", intType])
      (purePi "" intType intType)
      (ExprUtil.makeApply
        (mkInferredGetDef "id")
        ((mkInferredNode intType setType . Expression.BodyLeaf) Expression.Hole)
      )
    ) $
  mkInferredLeafSimple (Expression.LiteralInteger 5) intType

idOnHole :: HUnit.Test
idOnHole =
  testInfer "id hole"
  (pureApply [pureGetDef "id", pureHole]) .
  mkInferredNode
    (pureApply [pureGetDef "id", pureHole])
    (purePi "" pureHole pureHole) .
  ExprUtil.makeApply
    (mkInferredGetDef "id") $
  inferredHole setType

forceMono :: HUnit.Test
forceMono =
  testInfer "id (id _ _)"
  (pureApply [pureGetDef "id", idHoleHole]) .
  mkInferredNode
    (pureApply [pureGetDef "id", idSetHole])
    (purePi "" idSetHole idSetHole) $
  ExprUtil.makeApply
    (mkInferredGetDef "id") $
  mkInferredNode
    idSetHole
    setType $
  ExprUtil.makeApply
    (mkInferredNode
      idSet
      (purePi "" setType setType)
      (ExprUtil.makeApply
        (mkInferredGetDef "id")
        (mkInferredLeaf Expression.Hole setType setType)
      )
    ) $
  mkInferredLeafSimple Expression.Hole setType
  where
    idHole = pureApply [pureGetDef "id", pureHole]
    idHoleHole = pureApply [idHole, pureHole]
    idSet = pureApply [pureGetDef "id", setType]
    idSetHole = pureApply [idSet, pureHole]

inferredHole :: PureExpr def -> InferResults def
inferredHole = mkInferredLeafSimple Expression.Hole

inferredSetType :: InferResults def
inferredSetType = mkInferredLeafSimple Expression.Set setType

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
  mkInferredNode fOfX (rtAppliedTo "x") $ ExprUtil.makeApply inferredF inferredX
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
      ExprUtil.makeApply inferredRT .
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
    Just pl = tExpr ^? lens . Expression.ePayload
  in
    void . evaluate . (`runStateT` inferContext) $
    doInferM (Infer.iPoint pl) newExpr

applyIdInt :: PureExprDefI t
applyIdInt =
  ExprUtil.pureExpression
  (ExprUtil.makeApply
    (pureGetDef "id")
    intType
  )

-- {g, x} =>
-- \(g:hole) -> IntToBoolFunc x
makeFunnyLambda ::
  String ->
  PureExprDefI t ->
  PureExprDefI t
makeFunnyLambda g =
  pureLambda g pureHole .
  ExprUtil.pureExpression .
  ExprUtil.makeApply (pureGetDef "IntToBoolFunc")

testInfer ::
  String -> PureExprDefI t ->
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

getRecursiveDef :: PureExprDefI t
getRecursiveDef =
  ExprUtil.pureExpression $ Lens.review ExprUtil.bodyDefinitionRef recursiveDefI

resumptionTests :: [HUnit.Test]
resumptionTests =
  [ testResume "resume with pi"
    (purePi "" pureHole pureHole) pureHole id
  , testResume "resume infer in apply func"
    (pureGetDef "id") (pureApply [pureHole, pureHole]) (apply . Expression.applyFunc)
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
    (lamBody . lamBody . lamBody . apply . Expression.applyArg)
  , testCase "ref to the def on the side" $
    let
      (exprD, inferContext) =
        doInfer_ $ pureLambda "" pureHole pureHole
      Just body = exprD ^? lamBody
      scope = Infer.nScope . Infer.iPoint $ body ^. Expression.ePayload
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
      mkInferredLeafSimple (Expression.GetVariable (Expression.DefinitionRef recursiveDefI)) $
      purePi "" pureHole pureHole
  ]
  where
    apply :: Lens.Traversal' (Expression def a) (Expression.Apply (Expression def a))
    apply = Expression.eBody . Expression._BodyApply
    lamBody :: Lens.Traversal' (Expression def a) (Expression def a)
    lamBody = Expression.eBody . Expression._BodyLam . Expression.lambdaResult

makeParameterRef :: String -> PureExpr def
makeParameterRef =
  ExprUtil.pureExpression . Lens.review ExprUtil.bodyParameterRef . Guid.fromString

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
    lam = Expression.eBody . Expression._BodyLam
    Just pl =
      origInferred ^?
      lam . Expression.lambdaParamType .
      lam . Expression.lambdaResult .
      Expression.ePayload
    resumptionPoint = Infer.iPoint pl
    (origInferred, origInferContext) = doInfer_ origExpr
    origExpr =
      pureLambda "x" (purePi "" pureHole pureHole) $
      pureApply [makeParameterRef "x", pureHole, pureHole]

emptyRecordTest :: HUnit.Test
emptyRecordTest =
  testInfer "empty record type infer" emptyRecordType $
  mkInferredNode emptyRecordType setType emptyRecordTypeBody
  where
    emptyRecordType = ExprUtil.pureExpression emptyRecordTypeBody
    emptyRecordTypeBody =
      Expression.BodyRecord $ Expression.Record Expression.Type mempty

tagType :: Expression def ()
tagType = ExprUtil.pureExpression $ Expression.BodyLeaf Expression.TagType

recordTest :: HUnit.Test
recordTest =
  testInfer "f a x:a = {x" lamA $
  mkInferredNode lamA piA $
  makeNamedLambda "a" (mkInferredLeafSimple Expression.Set setType) $
  mkInferredNode lamX piX $
  makeNamedLambda "x" (mkInferredGetParam "a" setType) $
  mkInferredNode recVal recType $
  Expression.BodyRecord $
  Expression.Record Expression.Val
  [( mkInferredLeafSimple fieldTagLeaf tagType
   , mkInferredGetParam "x" (pureGetParam "a")
   )]
  where
    lamA =
      pureLambda "a" setType lamX
    lamX =
      pureLambda "x" (pureGetParam "a") recVal
    recVal =
      ExprUtil.pureExpression $
      rec Expression.Val $
      pureGetParam "x"
    fieldTagLeaf = Expression.Tag fieldGuid
    rec k =
      Expression.BodyRecord . Expression.Record k .
      (:[]) . (,) (ExprUtil.pureExpression (Expression.BodyLeaf fieldTagLeaf))
    fieldGuid = Guid.fromString "field"
    piA =
      purePi "a" setType piX
    piX =
      purePi "x" (pureGetParam "a") recType
    recType =
      ExprUtil.pureExpression $
      rec Expression.Type $
      pureGetParam "a"

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
