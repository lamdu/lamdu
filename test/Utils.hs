{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Arrow (first)
import Control.Monad (void)
import Data.Functor.Identity (Identity(..))
import Data.Hashable (hash)
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
import qualified Editor.Data.Typed as Typed
import qualified System.Random as Random

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Data.ExpressionBody a -> String
showStructure = show . (fmap . const) Invisible

instance Show (Data.Expression ()) where
  show (Data.Expression guid value ()) = show guid ++ ":" ++ show value

hole :: Data.PureExpression
hole = mkExpr "hole" $ Data.ExpressionLeaf Data.Hole

mkExpr ::
  String -> Data.ExpressionBody Data.PureExpression ->
  Data.PureExpression
mkExpr = Data.pureExpression . Guid.fromString

makeApply :: [Data.PureExpression] -> Data.PureExpression
makeApply = foldl1 (fmap (mkExpr "") . Data.makeApply)

makeLambda :: String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makeLambda name paramType body = mkExpr name $ Data.makeLambda paramType body

makePi :: String -> Data.PureExpression -> Data.PureExpression -> Data.PureExpression
makePi name paramType body = mkExpr name $ Data.makePi paramType body

setType :: Data.PureExpression
setType = mkExpr "set" $ Data.ExpressionLeaf Data.Set

intType :: Data.PureExpression
intType = mkExpr "int" $ Data.ExpressionLeaf Data.IntegerType

literalInt :: Integer -> Data.PureExpression
literalInt i = mkExpr ("lit" ++ show i) $ Data.makeLiteralInteger i

getDefExpr :: String -> Data.PureExpression
getDefExpr name =
  mkExpr name . Data.makeDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

getParamExpr :: String -> Data.PureExpression
getParamExpr name =
  mkExpr ("GetParam_" ++ name) . Data.makeParameterRef $
  Guid.fromString name

data ConflictsAnnotation =
  ConflictsAnnotation
  (Data.PureExpression, [Typed.Error])
  (Data.PureExpression, [Typed.Error])

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts :: Data.Expression ConflictsAnnotation -> String
showExpressionWithConflicts =
  List.intercalate "\n" . go
  where
    go typedExpr =
      [ "Expr: " ++ show (Data.eGuid typedExpr) ++ ":" ++ showStructure expr
      , "  IVal:  " ++ show val
      ] ++ map ((("    " ++ ansiRed ++ "Conflict: ") ++) . (++ ansiReset) . show) vErrors ++
      [ "  IType: " ++ show typ
      ] ++ map ((("    " ++ ansiRed ++ "Conflict: ") ++) . (++ ansiReset) . show) tErrors ++
      (map ("  " ++) . Foldable.concat . fmap go) expr
      where
        expr = Data.eValue typedExpr
        ConflictsAnnotation (val, vErrors) (typ, tErrors) =
          Data.ePayload typedExpr

definitionTypes :: Map Guid Data.PureExpression
definitionTypes =
  Map.fromList $ map (first Guid.fromString . randomizeGuids)
  [ ("Bool", setType)
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
  ]
  where
    randomizeGuids (name, expr) = (name, Data.randomizeGuids (Random.mkStdGen (hash name)) expr)
    intToIntToInt = makePi "iii0" intType $ makePi "iii1" intType intType

lookupMany :: Eq a => a -> [(a, b)] -> [b]
lookupMany x = map snd . filter ((== x) . fst)

doInferM ::
  Typed.RefMap -> Typed.InferNode -> Maybe Data.DefinitionIRef ->
  Maybe (Typed.Expression ()) ->
  Data.PureExpression ->
  (Typed.Expression (), Typed.RefMap)
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
      (Typed.iValue inferred, lookupMany valRef conflicts)
      (Typed.iType inferred, lookupMany typRef conflicts)
      where
        Typed.TypedValue valRef typRef = Typed.nRefs $ Typed.iPoint inferred
    root = fromMaybe inferredExpr mResumedRoot
    actions = Typed.InferActions reportError
    loaded = runIdentity $ Typed.load loader refMap inferNode mDefRef expr
    (result@(inferredExpr, _), conflicts) =
      Writer.runWriter $ Typed.infer actions loaded
    reportError err = Writer.tell [(Typed.errRef err, err)]

loader :: Monad m => Typed.Loader m
loader = Typed.Loader (return . (definitionTypes !) . IRef.guid)

doInfer :: Data.PureExpression -> (Typed.Expression (), Typed.RefMap)
doInfer = uncurry doInferM Typed.initial Nothing Nothing

factorialExpr :: Data.PureExpression
factorialExpr =
  Data.canonizeGuids .
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
      [ mkExpr "recurse" $ Data.makeDefinitionRef factorialDefI
      , makeApply [getDefExpr "-", getParamExpr "x", literalInt 1]
      ]
    ]
  ]

factorialDefI :: Data.DefinitionIRef
factorialDefI = IRef.unsafeFromGuid $ Guid.fromString "factorial"

factorial :: Int -> (Typed.Expression (), Typed.RefMap)
factorial gen =
  fromMaybe (error "Conflicts in factorial infer") $
  Typed.infer (Typed.InferActions (const Nothing)) loaded
  where
    loaded =
      runIdentity .
      uncurry (Typed.load loader) Typed.initial (Just factorialDefI) .
      void $ fmap (const gen) factorialExpr
