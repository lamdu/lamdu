{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Arrow (first)
import Control.Lens ((^.))
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
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Data.ExpressionBody DataIRef.DefinitionIRef a -> String
showStructure = show . (fmap . const) Invisible

instance Show (Data.Expression DataIRef.DefinitionIRef ()) where
  show (Data.Expression value ()) = show value

hole :: Data.Expression DataIRef.DefinitionIRef ()
hole = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

makeApply ::
  [Data.Expression DataIRef.DefinitionIRef ()] ->
  Data.Expression DataIRef.DefinitionIRef ()
makeApply = foldl1 (fmap Data.pureExpression . Data.makeApply)

makeLambdaCons ::
  (Guid -> Data.Expression DataIRef.DefinitionIRef () -> Data.Expression DataIRef.DefinitionIRef () ->
   Data.ExpressionBodyExpr DataIRef.DefinitionIRef ()) ->
  String -> Data.Expression DataIRef.DefinitionIRef () -> Data.Expression DataIRef.DefinitionIRef () ->
  Data.Expression DataIRef.DefinitionIRef ()
makeLambdaCons cons name paramType body =
  Data.pureExpression $ cons (Guid.fromString name) paramType body

makeLambda ::
  String -> Data.Expression DataIRef.DefinitionIRef () ->
  Data.Expression DataIRef.DefinitionIRef () ->
  Data.Expression DataIRef.DefinitionIRef ()
makeLambda = makeLambdaCons Data.makeLambda

makePi ::
  String -> Data.Expression DataIRef.DefinitionIRef () ->
  Data.Expression DataIRef.DefinitionIRef () ->
  Data.Expression DataIRef.DefinitionIRef ()
makePi = makeLambdaCons Data.makePi

setType :: Data.Expression DataIRef.DefinitionIRef ()
setType = Data.pureExpression $ Data.ExpressionLeaf Data.Set

intType :: Data.Expression DataIRef.DefinitionIRef ()
intType = Data.pureExpression $ Data.ExpressionLeaf Data.IntegerType

literalInt :: Integer -> Data.Expression DataIRef.DefinitionIRef ()
literalInt i = Data.pureExpression $ Data.makeLiteralInteger i

getDefExpr :: String -> Data.Expression DataIRef.DefinitionIRef ()
getDefExpr name =
  Data.pureExpression . Data.makeDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

getParamExpr :: String -> Data.Expression DataIRef.DefinitionIRef ()
getParamExpr name =
  Data.pureExpression . Data.makeParameterRef $
  Guid.fromString name

data ConflictsAnnotation =
  ConflictsAnnotation
  (Data.Expression DataIRef.DefinitionIRef (), [Infer.Error])
  (Data.Expression DataIRef.DefinitionIRef (), [Infer.Error])

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts ::
  Data.Expression DataIRef.DefinitionIRef ConflictsAnnotation -> String
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
        ConflictsAnnotation (val, vErrors) (typ, tErrors) =
          inferredExpr ^. Data.ePayload

definitionTypes :: Map Guid (Data.Expression DataIRef.DefinitionIRef ())
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
  Infer.RefMap -> Infer.InferNode -> Maybe DataIRef.DefinitionIRef ->
  Maybe (Infer.Expression a) ->
  Data.Expression DataIRef.DefinitionIRef a ->
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

defI :: DataIRef.DefinitionIRef
defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"

doInfer ::
  Data.Expression DataIRef.DefinitionIRef a -> (Infer.Expression a, Infer.RefMap)
doInfer = uncurry doInferM Infer.initial (Just defI) Nothing

factorialExpr :: Data.Expression DataIRef.DefinitionIRef ()
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

factorialDefI :: DataIRef.DefinitionIRef
factorialDefI = IRef.unsafeFromGuid $ Guid.fromString "factorial"

inferMaybe ::
  Maybe DataIRef.DefinitionIRef ->
  Data.Expression DataIRef.DefinitionIRef a ->
  Maybe (Infer.Expression a, Infer.RefMap)
inferMaybe mRecursiveDef expr =
  uncurry (Infer.infer (Infer.InferActions (const Nothing)) loaded) Infer.initial
  where
    loaded = runIdentity $ Infer.load loader mRecursiveDef expr

factorial :: Int -> (Infer.Expression (), Infer.RefMap)
factorial gen =
  fromMaybe (error "Conflicts in factorial infer") .
  inferMaybe (Just factorialDefI) . void $
  fmap (const gen) factorialExpr
