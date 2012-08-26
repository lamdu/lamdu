import Control.Applicative (liftA2)
import Control.Arrow (first, (***))
import Control.Exception (evaluate, onException)
import Control.Monad (void)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import System.IO (stderr, hPutStrLn)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import qualified Test.Framework as TestFramework

setType :: Data.PureExpression
setType = mkExpr "set" $ Data.ExpressionLeaf Data.Set

intType :: Data.PureExpression
intType =
  mkExpr "int" $ Data.ExpressionLeaf Data.IntegerType

makeDefinitionRef :: Guid -> Data.ExpressionBody a
makeDefinitionRef = Data.ExpressionLeaf . Data.GetVariable . Data.DefinitionRef . IRef.unsafeFromGuid

boolType :: Data.PureExpression
boolType = mkExpr "bool" . makeDefinitionRef $ Guid.fromString "Bool"

getDefExpr :: String -> Data.PureExpression
getDefExpr name =
  mkExpr name . Data.ExpressionLeaf .
  Data.GetVariable . Data.DefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

getParamExpr :: String -> Data.PureExpression
getParamExpr name =
  mkExpr ("GetParam: " ++ name) . Data.ExpressionLeaf .
  Data.GetVariable . Data.ParameterRef $ Guid.fromString name

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

definitionTypes :: Map Guid Data.PureExpression
definitionTypes =
  Map.fromList $ (map . first) Guid.fromString
  [ ("Bool", setType)
  , ("IntToBoolFunc", makePi "intToBool" intType boolType)
  , ( "id"
    , makePi "idType" setType $
      makePi "idGivenType" idTypeParam idTypeParam
    )
  ]
  where
    idTypeParam = mkExpr "idTypeParam" . Data.makeParameterRef $ Guid.fromString "idType"

doInferM ::
  Typed.RefMap -> Typed.InferNode -> Maybe Data.DefinitionIRef ->
  Data.PureExpression ->
  (Typed.Expression (), Typed.RefMap)
doInferM refMap inferNode mDefRef =
  fromMaybe (error "Infer failed!") .
  Typed.inferFromEntity loader (Typed.InferActions mempty) refMap inferNode mDefRef
  where
    loader = Typed.Loader (return . (definitionTypes !) . IRef.guid)

doInfer :: Data.PureExpression -> (Typed.Expression (), Typed.RefMap)
doInfer = uncurry doInferM Typed.initial Nothing

mkExpr ::
  String -> Data.ExpressionBody Data.PureExpression ->
  Data.PureExpression
mkExpr = Data.pureExpression . Guid.fromString

hole :: Data.PureExpression
hole = mkExpr "hole" $ Data.ExpressionLeaf Data.Hole

type InferResults = Data.Expression (Data.PureExpression, Data.PureExpression)

inferResults :: Typed.Expression a -> InferResults
inferResults =
  fmap f
  where
    f inferred = (Typed.iValue inferred, Typed.iType inferred)

showExpressionWithInferred :: InferResults -> String
showExpressionWithInferred =
  List.intercalate "\n" . go
  where
    go typedExpr =
      [ "Expr: " ++ show (Data.eGuid typedExpr) ++ ":" ++ show (void expr)
      , "  IVal:  " ++ show val
      , "  IType: " ++ show typ
      ] ++
      (map ("  " ++) . Foldable.concat .
       fmap go) expr
      where
        expr = Data.eValue typedExpr
        (val, typ) = Data.ePayload typedExpr

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

simpleTests :: [TestFramework.Test]
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

applyIntToBoolFuncWithHole :: TestFramework.Test
applyIntToBoolFuncWithHole =
  testInfer "apply"
  (makeApply [getDefExpr "IntToBoolFunc", hole]) $
  mkInferredNode ""
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    boolType $
  Data.makeApply
    (mkInferredGetDef "IntToBoolFunc")
    (inferredHole intType)

applyOnVar :: TestFramework.Test
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
    (makePi "" hole boolType) $
  Data.makeLambda (inferredHole setType) $
  mkInferredNode ""
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    boolType $
  Data.makeApply (mkInferredGetDef "IntToBoolFunc") $
  mkInferredNode ""
    hole
    intType $
  Data.makeApply
    (inferredHole (makePi "" hole intType)) $
  mkInferredLeafSimple
    ((Data.GetVariable . Data.ParameterRef . Guid.fromString) "lambda")
    hole

idTest :: TestFramework.Test
idTest =
  testInfer "id test"
  applyIdInt $
  mkInferredNode ""
    applyIdInt
    (makePi "" intType intType) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredLeafSimple Data.IntegerType setType

argTypeGoesToPi :: TestFramework.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi"
  (makeApply
    [ hole
    , mkExpr "" . Data.ExpressionLeaf $ Data.LiteralInteger 5
    ]
  ) $
  mkInferredNode ""
    hole
    hole $
  Data.makeApply
    (inferredHole (makePi "" intType hole)) $
  mkInferredLeafSimple (Data.LiteralInteger 5) intType

idOnAnInt :: TestFramework.Test
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

idOnHole :: TestFramework.Test
idOnHole =
  testInfer "id hole"
  (makeApply [getDefExpr "id", hole]) .
  mkInferredNode ""
    (makeApply [getDefExpr "id", hole])
    (makePi "" hole hole) .
  Data.makeApply
    (mkInferredGetDef "id") $
  inferredHole setType

inferredHole :: Data.PureExpression -> InferResults
inferredHole = mkInferredLeafSimple Data.Hole

-- | depApply =  \(t : Set) -> \(rt : t -> Set) -> \(f : (d : t) -> rt d) -> \(x : t) -> f x
depApply :: TestFramework.Test
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

main :: IO ()
main = TestFramework.defaultMain $
  simpleTests ++
  [ applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnHole
  , depApply
  ] ++
  resumptionTests

resumptionTests :: [TestFramework.Test]
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
  , testCase "ref to the def on the side" $
    let
      defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"
      (exprD, refMap) =
        uncurry doInferM Typed.initial (Just defI) $
        makeLambda "" hole hole
      Data.ExpressionLambda (Data.Lambda _ body) = Data.eValue exprD
      scope = Typed.nScope . Typed.iPoint $ Data.ePayload body
      (exprR, _) =
        uncurry doInferM (Typed.newNodeWithScope scope refMap) Nothing .
        mkExpr "" . Data.ExpressionLeaf . Data.GetVariable $ Data.DefinitionRef defI
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

evaluateMsg :: String -> a -> IO ()
evaluateMsg msg x = do
  _ <- evaluate x `onException` hPutStrLn stderr msg
  return ()

testResume ::
  TestFramework.TestName -> Data.PureExpression -> Data.PureExpression ->
  (Typed.Expression () -> Data.Expression (Typed.Inferred a)) ->
  TestFramework.Test
testResume name newExpr testExpr extract =
  testCase name $
  let
    (tExpr, refMap) = doInfer testExpr
  in
    evaluateMsg (showExpressionWithInferred (inferResults tExpr)) $
    doInferM refMap ((Typed.iPoint . Data.ePayload . extract) tExpr) Nothing newExpr

makeApply :: [Data.PureExpression] -> Data.PureExpression
makeApply = foldl1 (fmap (mkExpr "") . Data.makeApply)

makeLambda :: String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makeLambda name paramType body = mkExpr name $ Data.makeLambda paramType body

makePi :: String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makePi name paramType body = mkExpr name $ Data.makePi paramType body

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
  TestFramework.TestName -> Data.PureExpression ->
  InferResults -> TestFramework.Test
testInfer name pureExpr result =
  testCase name .
  assertBool
    (unlines
     [ "result expr:"  , showCanonized typedExpr
     , "expected expr:", showCanonized result
     ]) $ compareInferred typedExpr result
  where
    showCanonized =
      showExpressionWithInferred .
      fmap (Data.canonizeGuids *** Data.canonizeGuids) .
      Data.canonizeGuids
    typedExpr = inferResults . fst $ doInfer pureExpr
