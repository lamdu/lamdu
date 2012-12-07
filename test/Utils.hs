{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad (join, void)
import Control.Monad.Trans.State (State, runState, runStateT)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Lamdu.Data.IRef (DefI)
import Lamdu.Data.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.Infer as Infer

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Show def => Data.ExpressionBody def a -> String
showStructure = show . (Invisible <$)

instance Show def => Show (Data.Expression def ()) where
  show (Data.Expression value ()) = show value

makeNamedLambda :: String -> expr -> expr -> Data.ExpressionBody def expr
makeNamedLambda = Data.makeLambda . Guid.fromString

makeNamedPi :: String -> expr -> expr -> Data.ExpressionBody def expr
makeNamedPi = Data.makePi . Guid.fromString

pureApply :: [Data.Expression def ()] -> Data.Expression def ()
pureApply = foldl1 (fmap Data.pureExpression . Data.makeApply)

pureLambda ::
  String -> Data.Expression def () ->
  Data.Expression def () ->
  Data.Expression def ()
pureLambda name x y = Data.pureExpression $ makeNamedLambda name x y

purePi ::
  String -> Data.Expression def () ->
  Data.Expression def () ->
  Data.Expression def ()
purePi name x y = Data.pureExpression $ makeNamedPi name x y

hole :: Data.Expression def ()
hole = Data.pureHole

setType :: Data.Expression def ()
setType = Data.pureSet

intType :: Data.Expression def ()
intType = Data.pureIntegerType

literalInt :: Integer -> Data.Expression def ()
literalInt i = Data.pureExpression $ Data.makeLiteralInteger i

pureGetDef :: String -> Data.Expression (DefI t) ()
pureGetDef name =
  Data.pureExpression . Data.makeDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

pureGetParam :: String -> Data.Expression def ()
pureGetParam name =
  Data.pureExpression . Data.makeParameterRef $
  Guid.fromString name

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts ::
  Show def => Data.Expression def (InferredWithConflicts def) -> String
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

definitionTypes :: Map Guid (Data.Expression (DefI t) ())
definitionTypes =
  Map.fromList $ map (first Guid.fromString)
  [ ("Bool", setType)
  , ("List", purePi "list" setType setType)
  , ("IntToBoolFunc", purePi "intToBool" intType (pureGetDef "Bool"))
  , ("*", intToIntToInt)
  , ("-", intToIntToInt)
  , ( "if"
    , purePi "a" setType .
      purePi "ifboolarg" (pureGetDef "Bool") .
      purePi "iftarg" (pureGetParam "a") .
      purePi "iffarg" (pureGetParam "a") $
      pureGetParam "a"
    )
  , ( "=="
    , purePi "==0" intType .
      purePi "==1" intType $
      pureGetDef "Bool"
    )
  , ( "id"
    , purePi "a" setType $
      purePi "idGivenType" (pureGetParam "a") (pureGetParam "a")
    )
  , ( ":"
    , purePi "a" setType .
      purePi "consx" (pureGetParam "a") .
      join (purePi "consxs") $
      pureApply [pureGetDef "List", pureGetParam "a"]
    )
  ]
  where
    intToIntToInt = purePi "iii0" intType $ purePi "iii1" intType intType

doInferM ::
  Infer.InferNode (DefI t) -> Data.Expression (DefI t) a ->
  State (Infer.Context (DefI t)) (Data.Expression (DefI t) (Infer.Inferred (DefI t), a))
doInferM inferNode expr = do
  (success, exprWC) <-
    inferWithConflicts (doLoad expr) inferNode
  return $
    if success
    then (fmap . first) iwcInferred exprWC
    else
      error $ unlines
      [ "Result with conflicts:"
      -- TODO: Extract the real root (in case of resumption) to show
      , showExpressionWithConflicts $ fst <$> exprWC
      ]

doInferM_ ::
  Infer.InferNode (DefI t) -> Data.Expression (DefI t) a ->
  State (Infer.Context (DefI t)) (Data.Expression (DefI t) (Infer.Inferred (DefI t)))
doInferM_ = (fmap . fmap . fmap . fmap) fst doInferM

doLoad :: Data.Expression (DefI t) a -> Infer.Loaded (DefI t) a
doLoad expr =
  case Infer.load loader (Just defI) expr of
  Left err -> error err
  Right x -> x

loader :: Infer.Loader (DefI t) (Either String)
loader =
  Infer.Loader load
  where
    load key =
      case Map.lookup (IRef.guid key) definitionTypes of
      Nothing -> Left ("Could not find" ++ show key)
      Just x -> Right x

defI :: DefI t
defI = IRef.unsafeFromGuid $ Guid.fromString "Definition"

doInfer ::
  Data.Expression (DefI t) a ->
  ( Data.Expression (DefI t) (Infer.Inferred (DefI t), a)
  , Infer.Context (DefI t)
  )
doInfer =
  (`runState` ctx) . doInferM node
  where
    (ctx, node) = Infer.initial $ Just defI

doInfer_ ::
  Data.Expression (DefI t) a ->
  (Data.Expression (DefI t) (Infer.Inferred (DefI t)), Infer.Context (DefI t))
doInfer_ = (first . fmap) fst . doInfer

factorialExpr :: Data.Expression (DefI t) ()
factorialExpr =
  pureLambda "x" Data.pureHole $
  pureApply
  [ pureGetDef "if"
  , Data.pureHole
  , pureApply [pureGetDef "==", pureGetParam "x", literalInt 0]
  , literalInt 1
  , pureApply
    [ pureGetDef "*"
    , pureGetParam "x"
    , pureApply
      [ Data.pureExpression $ Data.makeDefinitionRef defI
      , pureApply [pureGetDef "-", pureGetParam "x", literalInt 1]
      ]
    ]
  ]

inferMaybe ::
  Data.Expression (DefI t) a ->
  Maybe (Data.Expression (DefI t) (Infer.Inferred (DefI t), a), Infer.Context (DefI t))
inferMaybe expr =
  (`runStateT` ctx) $
  Infer.inferLoaded (Infer.InferActions (const Nothing)) loaded node
  where
    (ctx, node) = Infer.initial (Just defI)
    loaded = doLoad expr

inferMaybe_ ::
  Data.Expression (DefI t) b ->
  Maybe (Data.Expression (DefI t) (Infer.Inferred (DefI t)), Infer.Context (DefI t))
inferMaybe_ = (fmap . first . fmap) fst . inferMaybe

factorial ::
  Int ->
  ( Data.Expression (DefI t) (Infer.Inferred (DefI t))
  , Infer.Context (DefI t)
  )
factorial gen =
  fromMaybe (error "Conflicts in factorial infer") .
  inferMaybe_ . void $
  fmap (const gen) factorialExpr
