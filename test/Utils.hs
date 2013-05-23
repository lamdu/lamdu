{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Applicative ((<$), (<$>), (<*>))
import Control.Lens ((^.), (%~))
import Control.Monad (void)
import Control.Monad.Trans.State (State, runState, runStateT)
import Data.Binary.Utils (encodeS)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import Data.Store.Guid (Guid)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import Lamdu.ExampleDB (createBuiltins)
import qualified Control.Lens as Lens
import qualified Data.ByteString as SBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type PureExpr def = Expression.Expression def ()

(==>) :: k -> v -> Map k v
(==>) = Map.singleton

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Show def => Expression.Body def a -> String
showStructure = show . (Invisible <$)

instance Show def => Show (PureExpr def) where
  show (Expression.Expression value ()) = show value

makeNamedLambda :: String -> expr -> expr -> Expression.Body def expr
makeNamedLambda = ExprUtil.makeLambda . Guid.fromString

makeNamedPi :: String -> expr -> expr -> Expression.Body def expr
makeNamedPi = ExprUtil.makePi . Guid.fromString

pureApply :: [PureExpr def] -> PureExpr def
pureApply = foldl1 (fmap ExprUtil.pureExpression . ExprUtil.makeApply)

-- 1 dependent param
pureApplyPoly1 ::
  String ->
  [Expression.Expression (DefI t) ()] ->
  Expression.Expression (DefI t) ()
pureApplyPoly1 name xs = pureApply $ pureGetDef name : pureHole : xs

pureLambda ::
  String -> PureExpr def ->
  PureExpr def ->
  PureExpr def
pureLambda name x y = ExprUtil.pureExpression $ makeNamedLambda name x y

purePi ::
  String -> PureExpr def ->
  PureExpr def ->
  PureExpr def
purePi name x y = ExprUtil.pureExpression $ makeNamedPi name x y

pureHole :: PureExpr def
pureHole = ExprUtil.pureHole

setType :: PureExpr def
setType = ExprUtil.pureSet

intType :: PureExpr def
intType = ExprUtil.pureIntegerType

literalInt :: Integer -> PureExpr def
literalInt = ExprUtil.pureExpression . Lens.review ExprUtil.bodyLiteralInteger

pureGetDef :: String -> DataIRef.Expression t ()
pureGetDef name =
  ExprUtil.pureExpression . Lens.review ExprUtil.bodyDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

pureGetParam :: String -> PureExpr def
pureGetParam name =
  ExprUtil.pureExpression . Lens.review ExprUtil.bodyParameterRef $
  Guid.fromString name

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts ::
  Show def => Expression.Expression def (InferredWithConflicts def) -> String
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
        expr = inferredExpr ^. Expression.eBody
        val = Infer.iValue inferred
        typ = Infer.iType inferred
        InferredWithConflicts inferred tErrors vErrors =
          inferredExpr ^. Expression.ePayload

definitionTypes :: Map Guid (DataIRef.Expression t ())
definitionTypes =
  exampleDBDefs `mappend` extras
  where
    g = Guid.fromString
    extras =
      mconcat
      [ g "IntToBoolFunc" ==> purePi "intToBool" intType (pureGetDef "Bool")
      ]
    exampleDBDefs =
      fst . MapStore.runEmpty . Transaction.run MapStore.mapStore $ do
        (_, defIs) <- createBuiltins
        Lens.mapMOf (Lens.traversed . ExprUtil.expressionDef) reIRef
          =<< Map.fromList <$> mapM readDef defIs

    reIRef = fmap IRef.unsafeFromGuid . guidNameOf
    guidNameOf =
      fmap Guid.fromString . Transaction.getP . Anchors.assocNameRef . IRef.guid
    readDef defI =
      (,)
      <$> guidNameOf defI
      <*>
      (fmap void . DataIRef.readExpression . Lens.view Definition.defType =<<
       Transaction.readIRef defI)

doInferM ::
  Infer.InferNode (DefI t) -> DataIRef.Expression t a ->
  State (Infer.Context (DefI t)) (DataIRef.Expression t (Infer.Inferred (DefI t), a))
doInferM inferNode expr = do
  (success, exprWC) <-
    inferWithConflicts (doLoad expr) inferNode
  return $
    if success
    then Lens.mapped . Lens._1 %~ iwcInferred $ exprWC
    else
      error $ unlines
      [ "Result with conflicts:"
      -- TODO: Extract the real root (in case of resumption) to show
      , showExpressionWithConflicts $ fst <$> exprWC
      ]

doInferM_ ::
  Infer.InferNode (DefI t) -> DataIRef.Expression t a ->
  State (Infer.Context (DefI t)) (DataIRef.Expression t (Infer.Inferred (DefI t)))
doInferM_ = (fmap . fmap . fmap . fmap) fst doInferM

doLoad :: DataIRef.Expression t a -> Infer.Loaded (DefI t) a
doLoad expr =
  case Infer.load loader (Just recursiveDefI) expr of
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

recursiveDefI :: DefI t
recursiveDefI = IRef.unsafeFromGuid $ Guid.fromString "Definition"

doInfer ::
  DataIRef.Expression t a ->
  ( DataIRef.Expression t (Infer.Inferred (DefI t), a)
  , Infer.Context (DefI t)
  )
doInfer =
  (`runState` ctx) . doInferM node
  where
    (ctx, node) = Infer.initial $ Just recursiveDefI

doInfer_ ::
  DataIRef.Expression t a ->
  (DataIRef.Expression t (Infer.Inferred (DefI t)), Infer.Context (DefI t))
doInfer_ = (Lens._1 . Lens.mapped %~ fst) . doInfer

pureGetRecursiveDefI :: DataIRef.Expression t ()
pureGetRecursiveDefI =
  ExprUtil.pureExpression $ Lens.review ExprUtil.bodyDefinitionRef recursiveDefI

factorialExpr :: DataIRef.Expression t ()
factorialExpr =
  pureLambda "x" pureHole $
  pureApplyPoly1 "if"
  [ pureApplyPoly1 "==" [pureGetParam "x", literalInt 0]
  , literalInt 1
  , pureApplyPoly1 "*"
    [ pureGetParam "x"
    , pureApply
      [ pureGetRecursiveDefI
      , pureApplyPoly1 "-" [pureGetParam "x", literalInt 1]
      ]
    ]
  ]

euler1Expr :: DataIRef.Expression t ()
euler1Expr =
  pureApplyPoly1 "sum"
  [ pureApplyPoly1 "filter"
    [ pureLambda "x" pureHole $
      pureApply
      [ pureGetDef "||"
      , pureApplyPoly1 "=="
        [ literalInt 0, pureApplyPoly1 "%" [pureGetParam "x", literalInt 3] ]
      , pureApplyPoly1 "=="
        [ literalInt 0, pureApplyPoly1 "%" [pureGetParam "x", literalInt 5] ]
      ]
    , pureApply [pureGetDef "..", literalInt 1, literalInt 1000]
    ]
  ]

inferMaybe ::
  DataIRef.Expression t a ->
  Maybe (DataIRef.Expression t (Infer.Inferred (DefI t), a), Infer.Context (DefI t))
inferMaybe expr =
  (`runStateT` ctx) $
  Infer.inferLoaded (Infer.InferActions (const Nothing)) loaded node
  where
    (ctx, node) = Infer.initial (Just recursiveDefI)
    loaded = doLoad expr

inferMaybe_ ::
  DataIRef.Expression t b ->
  Maybe (DataIRef.Expression t (Infer.Inferred (DefI t)), Infer.Context (DefI t))
inferMaybe_ = (Lens.mapped . Lens._1 . Lens.mapped %~ fst) . inferMaybe

inferAndEncode :: DataIRef.Expression t a -> Int -> Int
inferAndEncode expr par = SBS.length . encodeS $ fst result
  where
    result =
      fromMaybe (error "Conflicts in factorial infer") .
      inferMaybe_ . void $
      fmap (const par) expr

factorial :: Int -> Int
factorial = inferAndEncode factorialExpr

euler1 :: Int -> Int
euler1 = inferAndEncode euler1Expr
