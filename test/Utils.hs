{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad (join, void)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Editor.Data.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer

type DefI = DataIRef.DefinitionIRef

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Data.ExpressionBody DefI a -> String
showStructure = show . (Invisible <$)

instance Show (Data.Expression DefI ()) where
  show (Data.Expression value ()) = show value

hole :: Data.Expression DefI ()
hole = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

makeApply ::
  [Data.Expression DefI ()] ->
  Data.Expression DefI ()
makeApply = foldl1 (fmap Data.pureExpression . Data.makeApply)

makeLambdaCons ::
  (Guid -> Data.Expression DefI () -> Data.Expression DefI () ->
   Data.ExpressionBodyExpr DefI ()) ->
  String -> Data.Expression DefI () -> Data.Expression DefI () ->
  Data.Expression DefI ()
makeLambdaCons cons name paramType body =
  Data.pureExpression $ cons (Guid.fromString name) paramType body

makeLambda ::
  String -> Data.Expression DefI () ->
  Data.Expression DefI () ->
  Data.Expression DefI ()
makeLambda = makeLambdaCons Data.makeLambda

makePi ::
  String -> Data.Expression DefI () ->
  Data.Expression DefI () ->
  Data.Expression DefI ()
makePi = makeLambdaCons Data.makePi

setType :: Data.Expression DefI ()
setType = Data.pureExpression $ Data.ExpressionLeaf Data.Set

intType :: Data.Expression DefI ()
intType = Data.pureExpression $ Data.ExpressionLeaf Data.IntegerType

literalInt :: Integer -> Data.Expression DefI ()
literalInt i = Data.pureExpression $ Data.makeLiteralInteger i

getDefExpr :: String -> Data.Expression DefI ()
getDefExpr name =
  Data.pureExpression . Data.makeDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

getParamExpr :: String -> Data.Expression DefI ()
getParamExpr name =
  Data.pureExpression . Data.makeParameterRef $
  Guid.fromString name

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts ::
  Data.Expression DefI (InferredWithConflicts DefI a) -> String
showExpressionWithConflicts =
  List.intercalate "\n" . go
  where
    go inferredExpr =
      [ "Expr:" ++ showStructure expr
      , "  IVal:  " ++ show val
      ] ++ map ((("    " ++ ansiRed ++ "Conflict: ") ++) . (++ ansiReset) . show) vErrors ++
      [ "  IType: " ++ show typ
      ] ++ map ((("    " ++ ansiRed ++ "Conflict: ") ++) . (++ ansiReset) . show) tErrors ++
      (map ("  " ++) . Foldable.concat . fmap go) expr
      where
        expr = inferredExpr ^. Data.eValue
        val = Infer.iValue inferred
        typ = Infer.iType inferred
        InferredWithConflicts inferred tErrors vErrors =
          inferredExpr ^. Data.ePayload

definitionTypes :: Map Guid (Data.Expression DefI ())
definitionTypes =
  Map.fromList $ map (first Guid.fromString)
  [ ("Bool", setType)
  , ("List", makePi "list" setType setType)
  , ("IntToBoolFunc", makePi "intToBool" intType (getDefExpr "Bool"))
  , ("*", intToIntToInt)
  , ("-", intToIntToInt)
  , ( "if"
    , makePi "a" setType .
      makePi "ifboolarg" (getDefExpr "Bool") .
      makePi "iftarg" (getParamExpr "a") .
      makePi "iffarg" (getParamExpr "a") $
      getParamExpr "a"
    )
  , ( "=="
    , makePi "==0" intType .
      makePi "==1" intType $
      getDefExpr "Bool"
    )
  , ( "id"
    , makePi "a" setType $
      makePi "idGivenType" (getParamExpr "a") (getParamExpr "a")
    )
  , ( ":"
    , makePi "a" setType .
      makePi "consx" (getParamExpr "a") .
      join (makePi "consxs") $
      makeApply [getDefExpr "List", getParamExpr "a"]
    )
  ]
  where
    intToIntToInt = makePi "iii0" intType $ makePi "iii1" intType intType

doInferM ::
  Infer.Context DefI -> Infer.InferNode DefI ->
  Data.Expression DefI a ->
  (Data.Expression DefI (Infer.Inferred DefI a), Infer.Context DefI)
doInferM initialInferContext inferNode expr
  | success = (iwcInferred <$> exprWC, resultInferContext)
  | otherwise =
    error $ unlines
    [ "Result with conflicts:"
    -- TODO: Extract the real root (in case of resumption) to show
    , showExpressionWithConflicts exprWC
    ]
  where
    (success, resultInferContext, exprWC) =
      inferWithConflicts (doLoad expr) initialInferContext inferNode

doLoad ::
  Data.Expression DefI a ->
  Infer.Loaded DefI a
doLoad expr =
  case Infer.load loader (Just defI) expr of
  Left err -> error err
  Right x -> x

loader :: Infer.Loader DefI (Either String)
loader =
  Infer.Loader load
  where
    load key =
      case Map.lookup (IRef.guid key) definitionTypes of
      Nothing -> Left ("Could not find" ++ show key)
      Just x -> Right x

defI :: DefI
defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"

doInfer ::
  Data.Expression DefI a -> (Data.Expression DefI (Infer.Inferred DefI a), Infer.Context DefI)
doInfer = uncurry doInferM . Infer.initial $ Just defI

factorialExpr :: Data.Expression DefI ()
factorialExpr =
  makeLambda "x" hole $
  makeApply
  [ getDefExpr "if"
  , hole
  , makeApply [getDefExpr "==", getParamExpr "x", literalInt 0]
  , literalInt 1
  , makeApply
    [ getDefExpr "*"
    , getParamExpr "x"
    , makeApply
      [ Data.pureExpression $ Data.makeDefinitionRef defI
      , makeApply [getDefExpr "-", getParamExpr "x", literalInt 1]
      ]
    ]
  ]

inferMaybe ::
  Data.Expression DefI a ->
  Maybe (Data.Expression DefI (Infer.Inferred DefI a), Infer.Context DefI)
inferMaybe expr =
  uncurry (Infer.inferLoaded (Infer.InferActions (const Nothing)) loaded) $ Infer.initial (Just defI)
  where
    loaded = doLoad expr

factorial :: Int -> (Data.Expression DefI (Infer.Inferred DefI ()), Infer.Context DefI)
factorial gen =
  fromMaybe (error "Conflicts in factorial infer") .
  inferMaybe . void $
  fmap (const gen) factorialExpr
