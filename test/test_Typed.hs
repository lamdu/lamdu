import Control.Applicative (liftA2)
import Control.Arrow (first)
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
  Typed.RefMap -> Typed.InferNode -> Maybe (Data.DefinitionIRef) ->
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
      [ "Expr: " ++ show (fmap (const ()) expr)
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

main :: IO ()
main = TestFramework.defaultMain
  [ testInfer "literal int"
    (mkExpr "5" (Data.ExpressionLeaf (Data.LiteralInteger 5))) $
    mkInferredLeaf (Data.LiteralInteger 5) intType
  , testInfer "simple apply"
    (makeApply [hole, hole]) $
    mkInferredNode "" hole hole $
    Data.makeApply
      (mkInferredLeaf Data.Hole (mkExpr "" (Data.makePi hole hole)))
      (mkInferredLeaf Data.Hole hole)
  , testInfer "apply"
    (makeApply [getDefExpr "IntToBoolFunc", hole]) $
    mkInferredNode ""
      (makeApply [getDefExpr "IntToBoolFunc", hole])
      boolType $
    Data.makeApply
      (mkInferredGetDef "IntToBoolFunc")
      (mkInferredLeaf Data.Hole intType)
  , testInfer "apply on var"
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
  , testInfer "id test"
    applyIdInt $
    mkInferredNode ""
      applyIdInt
      (mkExpr "" (Data.makePi intType intType)) $
    Data.makeApply
      (mkInferredGetDef "id") $
    mkInferredLeaf Data.IntegerType setType
  , testInfer "arg type goes to pi"
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
  , testInfer "id on an int"
    (makeApply
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
  , testInfer "id hole"
    (makeApply [getDefExpr "id", hole]) $
    mkInferredNode ""
      (makeApply [getDefExpr "id", hole])
      (mkExpr "" (Data.makePi hole hole)) $
    Data.makeApply
      (mkInferredGetDef "id") $
    mkInferredLeaf Data.Hole setType
  , testCase "resume infer" $
    let
      (tExpr, refMap) = doInfer $ makeApply [hole, hole]
      Data.ExpressionApply (Data.Apply firstHole _) = Data.eValue tExpr
    in
      assertBool (showExpressionWithInferred (inferResults tExpr)) . isJust $
        doInferEx refMap ((Typed.iPoint . Data.ePayload) firstHole) Nothing $ getDefExpr "id"
  , testCase "ref to the def on the side" $
    let
      defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"
      Just (_, refMap) =
        uncurry doInferEx Typed.initial (Just defI) .
        mkExpr "" $ Data.makeLambda hole hole
      Just (tExpr, _) =
        uncurry doInferEx (Typed.newNode refMap) (Just defI) .
        mkExpr "" . Data.ExpressionLeaf . Data.GetVariable $ Data.DefinitionRef defI
      result = inferResults tExpr
    in
      assertBool (showExpressionWithInferred result) .
      compareInferred result .
      mkInferredLeaf (Data.GetVariable (Data.DefinitionRef defI)) $
      mkExpr "" $ Data.makePi hole hole
  ]
  where
    makeApply = foldl1 (fmap (mkExpr "") . Data.makeApply)
    applyIdInt =
      mkExpr ""
      (Data.makeApply
        (getDefExpr "id")
        intType
      )
    makeFunnyLambda g =
      mkExpr g .
      Data.makeLambda hole .
      mkExpr "applyFunny" .
      Data.makeApply (getDefExpr "IntToBoolFunc")
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
