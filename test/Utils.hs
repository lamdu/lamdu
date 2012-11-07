{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Arrow (first)
import Control.Monad (join, void)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Editor.Data as Data
import qualified Editor.Data.Infer as Infer

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Data.ExpressionBody a -> String
showStructure = show . (fmap . const) Invisible

instance Show (Data.Expression ()) where
  show (Data.Expression value ()) = show value

hole :: Data.PureExpression
hole = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

makeApply :: [Data.PureExpression] -> Data.PureExpression
makeApply = foldl1 (fmap Data.pureExpression . Data.makeApply)

makeLambdaCons ::
  (Guid -> Data.PureExpression -> Data.PureExpression -> Data.ExpressionBody Data.PureExpression) ->
  String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makeLambdaCons cons name paramType body =
  Data.pureExpression $ cons (Guid.fromString name) paramType body

makeLambda :: String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makeLambda = makeLambdaCons Data.makeLambda

makePi :: String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makePi = makeLambdaCons Data.makePi

setType :: Data.PureExpression
setType = Data.pureExpression $ Data.ExpressionLeaf Data.Set

intType :: Data.PureExpression
intType = Data.pureExpression $ Data.ExpressionLeaf Data.IntegerType

literalInt :: Integer -> Data.PureExpression
literalInt i = Data.pureExpression $ Data.makeLiteralInteger i

getDefExpr :: String -> Data.PureExpression
getDefExpr name =
  Data.pureExpression . Data.makeDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

getParamExpr :: String -> Data.PureExpression
getParamExpr name =
  Data.pureExpression . Data.makeParameterRef $
  Guid.fromString name

data ConflictsAnnotation =
  ConflictsAnnotation
  (Data.PureExpression, [Infer.Error])
  (Data.PureExpression, [Infer.Error])

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts :: Data.Expression ConflictsAnnotation -> String
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
        expr = Data.eValue inferredExpr
        ConflictsAnnotation (val, vErrors) (typ, tErrors) =
          Data.ePayload inferredExpr

definitionTypes :: Map Guid Data.PureExpression
definitionTypes =
  Map.fromList $ map (first Guid.fromString)
  [ ("Bool", setType)
  , ("List", makePi "list" setType setType)
  , ("IntToBoolFunc", makePi "intToBool" intType (getDefExpr "bool"))
  , ("*", intToIntToInt)
  , ("-", intToIntToInt)
  , ( "if"
    , makePi "a" setType .
      makePi "ifboolarg" (getDefExpr "bool") .
      makePi "iftarg" (getParamExpr "a") .
      makePi "iffarg" (getParamExpr "a") $
      getParamExpr "a"
    )
  , ( "=="
    , makePi "==0" intType .
      makePi "==1" intType $
      getDefExpr "bool"
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

lookupMany :: Eq a => a -> [(a, b)] -> [b]
lookupMany x = map snd . filter ((== x) . fst)

doInferM ::
  Infer.RefMap -> Infer.InferNode -> Maybe Data.DefinitionIRef ->
  Maybe (Infer.Expression a) ->
  Data.Expression a ->
  (Infer.Expression a, Infer.RefMap)
doInferM refMap inferNode mDefRef mResumedRoot expr =
  case conflicts of
    [] -> result
    _ -> error $ unlines
      [ "Result with conflicts:"
      , showExpressionWithConflicts $ fmap addConflicts root
      ]
  where
    addConflicts inferred =
      ConflictsAnnotation
      (Infer.iValue inferred, lookupMany valRef conflicts)
      (Infer.iType inferred, lookupMany typRef conflicts)
      where
        Infer.TypedValue valRef typRef = Infer.nRefs $ Infer.iPoint inferred
    root = fromMaybe inferredExpr mResumedRoot
    actions = Infer.InferActions reportError
    loaded = runIdentity $ Infer.load loader mDefRef expr
    (result@(inferredExpr, _), conflicts) =
      Writer.runWriter $ Infer.infer actions loaded refMap inferNode
    reportError err = Writer.tell [(Infer.errRef err, err)]

loader :: Monad m => Infer.Loader m
loader = Infer.Loader (return . (definitionTypes !) . IRef.guid)

defI :: Data.DefinitionIRef
defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"

doInfer :: Data.Expression a -> (Infer.Expression a, Infer.RefMap)
doInfer = uncurry doInferM Infer.initial (Just defI) Nothing

factorialExpr :: Data.PureExpression
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
      [ Data.pureExpression $ Data.makeDefinitionRef factorialDefI
      , makeApply [getDefExpr "-", getParamExpr "x", literalInt 1]
      ]
    ]
  ]

factorialDefI :: Data.DefinitionIRef
factorialDefI = IRef.unsafeFromGuid $ Guid.fromString "factorial"

inferMaybe :: Maybe Data.DefinitionIRef -> Data.Expression a -> Maybe (Infer.Expression a, Infer.RefMap)
inferMaybe mRecursiveDef expr =
  uncurry (Infer.infer (Infer.InferActions (const Nothing)) loaded) Infer.initial
  where
    loaded = runIdentity $ Infer.load loader mRecursiveDef expr

factorial :: Int -> (Infer.Expression (), Infer.RefMap)
factorial gen =
  fromMaybe (error "Conflicts in factorial infer") .
  inferMaybe (Just factorialDefI) . void $
  fmap (const gen) factorialExpr
