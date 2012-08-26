import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Monad (void)
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Guid as Guid
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

definitionTypes :: Map Guid Data.PureExpression
definitionTypes =
  Map.fromList $ (map . first) Guid.fromString
  [ ("Bool", setType)
  , ("IntToBoolFunc", mkExpr "intToBool" $ Data.makePi intType boolType)
  , ( "id"
    , mkExpr "idType" . Data.makePi setType .
      mkExpr "idGivenType" $ Data.makePi idTypeParam idTypeParam
    )
  ]
  where
    idTypeParam = mkExpr "idTypeParam" . Data.makeParameterRef $ Guid.fromString "idType"

doInferEx ::
  Typed.RefMap -> Typed.InferNode -> Maybe Data.DefinitionIRef ->
  Data.PureExpression ->
  Maybe (Typed.Expression (), Typed.RefMap)
doInferEx =
  Typed.inferFromEntity loader (Typed.InferActions mempty)
  where
    loader = Typed.Loader (return . (definitionTypes !) . IRef.guid)

doInfer ::
  Data.PureExpression ->
  (Typed.Expression (), Typed.RefMap)
doInfer =
  fromMaybe (error "doInfer failed!") .
  uncurry doInferEx Typed.initial Nothing

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

mkInferredLeaf :: Data.Leaf -> Data.PureExpression -> InferResults
mkInferredLeaf leaf typ =
  Data.Expression
  { Data.eGuid = Guid.fromString "leaf"
  , Data.eValue = Data.ExpressionLeaf leaf
  , Data.ePayload =
      (Data.pureExpression g (Data.ExpressionLeaf leaf), typ)
  }
  where
    g = Guid.fromString "leaf"

mkInferredGetDef :: String -> InferResults
mkInferredGetDef name =
  mkInferredLeaf
  (Data.GetVariable . Data.DefinitionRef $ IRef.unsafeFromGuid g)
  (definitionTypes ! g)
  where
    g = Guid.fromString name

mkInferredNode ::
  String -> Data.PureExpression -> Data.PureExpression ->
  Data.ExpressionBody InferResults -> InferResults
mkInferredNode g iVal iType body =
  Data.Expression (Guid.fromString g) body (iVal, iType)

simpleTests :: [TestFramework.Test]
simpleTests =
  [ testInfer "literal int"
    (mkExpr "5" (Data.ExpressionLeaf (Data.LiteralInteger 5))) $
    mkInferredLeaf (Data.LiteralInteger 5) intType
  , testInfer "simple apply"
    ((Data.canonizeGuids . makeApply) [hole, hole]) $
    mkInferredNode "" hole hole $
    Data.makeApply
      (mkInferredLeaf Data.Hole (mkExpr "" (Data.makePi hole hole)))
      (mkInferredLeaf Data.Hole hole)
  , testInfer "simple pi"
    (mkExpr "pi" (Data.makePi hole hole)) $
    mkInferredNode ""
      (mkExpr "pi" (Data.makePi hole hole))
      setType $
    Data.makePi
      (mkInferredLeaf Data.Hole setType)
      (mkInferredLeaf Data.Hole setType)
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
    (mkInferredLeaf Data.Hole intType)

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
    (mkExpr "" (Data.makePi hole boolType)) $
  Data.makeLambda (mkInferredLeaf Data.Hole setType) $
  mkInferredNode ""
    (makeApply [getDefExpr "IntToBoolFunc", hole])
    boolType $
  Data.makeApply (mkInferredGetDef "IntToBoolFunc") $
  mkInferredNode ""
    hole
    intType $
  Data.makeApply
    (mkInferredLeaf Data.Hole (mkExpr "" (Data.makePi hole intType))) $
  mkInferredLeaf
    ((Data.GetVariable . Data.ParameterRef . Guid.fromString) "lambda")
    hole

idTest :: TestFramework.Test
idTest =
  testInfer "id test"
  applyIdInt $
  mkInferredNode ""
    applyIdInt
    (mkExpr "" (Data.makePi intType intType)) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredLeaf Data.IntegerType setType

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
    (mkInferredLeaf Data.Hole (mkExpr "" (Data.makePi intType hole))) $
  mkInferredLeaf (Data.LiteralInteger 5) intType

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
      (mkExpr "" (Data.makePi intType intType))
      (Data.makeApply
        (mkInferredGetDef "id")
        ((mkInferredNode "" intType setType . Data.ExpressionLeaf) Data.Hole)
      )
    ) $
  mkInferredLeaf (Data.LiteralInteger 5) intType

idOnHole :: TestFramework.Test
idOnHole =
  testInfer "id hole"
  (makeApply [getDefExpr "id", hole]) $
  mkInferredNode ""
    (makeApply [getDefExpr "id", hole])
    (mkExpr "" (Data.makePi hole hole)) $
  Data.makeApply
    (mkInferredGetDef "id") $
  mkInferredLeaf Data.Hole setType

main :: IO ()
main = TestFramework.defaultMain $
  simpleTests ++
  [ applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnHole
  ] ++
  resumptionTests

resumptionTests :: [TestFramework.Test]
resumptionTests =
  [ testResume "resume with pi"
    (mkExpr "" (Data.makePi hole hole)) hole id
  , testResume "resume infer in apply func"
    (getDefExpr "id") (Data.canonizeGuids (makeApply [hole, hole])) getApplyFunc
  , testResume "resume infer in lambda body"
    (getDefExpr "id") (mkExpr "" (Data.makeLambda hole hole)) getLambdaBody
  , testResume "resume infer to get param 1 of 2"
    (getParamExpr "a")
    ((mkExpr "a" . Data.makeLambda hole . mkExpr "b" . Data.makeLambda hole) hole)
    (getLambdaBody . getLambdaBody)
  , testResume "resume infer to get param 2 of 2"
    (getParamExpr "b")
    ((mkExpr "a" . Data.makeLambda hole . mkExpr "b" . Data.makeLambda hole) hole)
    (getLambdaBody . getLambdaBody)
  , testCase "ref to the def on the side" $
    let
      defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"
      Just (exprD, refMap) =
        uncurry doInferEx Typed.initial (Just defI) .
        mkExpr "" $ Data.makeLambda hole hole
      Data.ExpressionLambda (Data.Lambda _ body) = Data.eValue exprD
      scope = Typed.nScope . Typed.iPoint $ Data.ePayload body
      Just (exprR, _) =
        uncurry doInferEx (Typed.newNodeWithScope scope refMap) Nothing .
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
      mkInferredLeaf (Data.GetVariable (Data.DefinitionRef defI)) $
      mkExpr "" $ Data.makePi hole hole
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

testResume ::
  TestFramework.TestName -> Data.PureExpression -> Data.PureExpression ->
  (Typed.Expression () -> Data.Expression (Typed.Inferred a)) ->
  TestFramework.Test
testResume name newExpr testExpr extract =
  testCase name $
  let
    (tExpr, refMap) = doInfer testExpr
  in
    assertBool (showExpressionWithInferred (inferResults tExpr)) . isJust $
      doInferEx refMap ((Typed.iPoint . Data.ePayload . extract) tExpr) Nothing $ newExpr

makeApply :: [Data.PureExpression] -> Data.PureExpression
makeApply = foldl1 (fmap (mkExpr "") . Data.makeApply)

applyIdInt :: Data.PureExpression
applyIdInt =
  mkExpr ""
  (Data.makeApply
    (getDefExpr "id")
    intType
  )

makeFunnyLambda :: String -> Data.PureExpression -> Data.PureExpression
makeFunnyLambda g =
  mkExpr g .
  Data.makeLambda hole .
  mkExpr "applyFunny" .
  Data.makeApply (getDefExpr "IntToBoolFunc")

testInfer ::
  TestFramework.TestName -> Data.PureExpression ->
  InferResults -> TestFramework.Test
testInfer name pureExpr result =
  testCase name .
  assertBool
    (unlines
     [ "Unexpected result expr:"
     , showExpressionWithInferred typedExpr
     , showExpressionWithInferred result
     ]) $ compareInferred typedExpr result
  where
    typedExpr = inferResults . fst $ doInfer pureExpr
