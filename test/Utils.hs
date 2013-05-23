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
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type PureExpr def = Expr.Expression def ()
type PureExprDefI t = PureExpr (DefI t)

(==>) :: k -> v -> Map k v
(==>) = Map.singleton

data Invisible = Invisible
instance Show Invisible where
  show = const ""
showStructure :: Show def => Expr.Body def a -> String
showStructure = show . (Invisible <$)

instance Show def => Show (PureExpr def) where
  show (Expr.Expression value ()) = show value

namedLambda :: String -> expr -> expr -> Expr.Body def expr
namedLambda = ExprUtil.makeLambda . Guid.fromString

namedPi :: String -> expr -> expr -> Expr.Body def expr
namedPi = ExprUtil.makePi . Guid.fromString

pureApply :: [PureExpr def] -> PureExpr def
pureApply = foldl1 (fmap ExprUtil.pureExpression . ExprUtil.makeApply)

-- 1 dependent param
pureApplyPoly1 ::
  String ->
  [Expr.Expression (DefI t) ()] ->
  Expr.Expression (DefI t) ()
pureApplyPoly1 name xs = pureApply $ pureGetDef name : pureHole : xs

pureLambda ::
  String -> PureExpr def ->
  PureExpr def ->
  PureExpr def
pureLambda name x y = ExprUtil.pureExpression $ namedLambda name x y

purePi ::
  String -> PureExpr def ->
  PureExpr def ->
  PureExpr def
purePi name x y = ExprUtil.pureExpression $ namedPi name x y

pureHole :: PureExpr def
pureHole = ExprUtil.pureHole

setType :: PureExpr def
setType = ExprUtil.pureSet

intType :: PureExpr def
intType = ExprUtil.pureIntegerType

literalInt :: Integer -> PureExpr def
literalInt = ExprUtil.pureExpression . Lens.review ExprLens.bodyLiteralInteger

pureGetDef :: String -> PureExprDefI t
pureGetDef name =
  ExprUtil.pureExpression . Lens.review ExprLens.bodyDefinitionRef . IRef.unsafeFromGuid $
  Guid.fromString name

pureGetParam :: String -> PureExpr def
pureGetParam name =
  ExprUtil.pureExpression . Lens.review ExprLens.bodyParameterRef $
  Guid.fromString name

ansiRed :: String
ansiRed = "\ESC[31m"
ansiReset :: String
ansiReset = "\ESC[0m"

showExpressionWithConflicts ::
  Show def => Expr.Expression def (InferredWithConflicts def) -> String
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
        expr = inferredExpr ^. Expr.eBody
        val = Infer.iValue inferred
        typ = Infer.iType inferred
        InferredWithConflicts inferred tErrors vErrors =
          inferredExpr ^. Expr.ePayload

definitionTypes :: Map Guid (PureExprDefI t)
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
        Lens.mapMOf (Lens.traversed . ExprLens.exprDef) reIRef
          =<< Map.fromList <$> mapM readDef defIs

    reIRef = fmap IRef.unsafeFromGuid . guidNameOf
    guidNameOf =
      fmap Guid.fromString . Transaction.getP . Anchors.assocNameRef . IRef.guid
    readDef defI =
      (,)
      <$> guidNameOf defI
      <*>
      (fmap void . ExprIRef.readExpression . Lens.view Definition.defType =<<
       Transaction.readIRef defI)

doInferM ::
  Infer.InferNode (DefI t) -> ExprIRef.Expression t a ->
  State (Infer.Context (DefI t)) (ExprIRef.Expression t (Infer.Inferred (DefI t), a))
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
  Infer.InferNode (DefI t) -> ExprIRef.Expression t a ->
  State (Infer.Context (DefI t)) (ExprIRef.Expression t (Infer.Inferred (DefI t)))
doInferM_ = (fmap . fmap . fmap . fmap) fst doInferM

doLoad :: ExprIRef.Expression t a -> Infer.Loaded (DefI t) a
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
  ExprIRef.Expression t a ->
  ( ExprIRef.Expression t (Infer.Inferred (DefI t), a)
  , Infer.Context (DefI t)
  )
doInfer =
  (`runState` ctx) . doInferM node
  where
    (ctx, node) = Infer.initial $ Just recursiveDefI

doInfer_ ::
  ExprIRef.Expression t a ->
  (ExprIRef.Expression t (Infer.Inferred (DefI t)), Infer.Context (DefI t))
doInfer_ = (Lens._1 . Lens.mapped %~ fst) . doInfer

pureGetRecursiveDefI :: PureExprDefI t
pureGetRecursiveDefI =
  ExprUtil.pureExpression $ Lens.review ExprLens.bodyDefinitionRef recursiveDefI

factorialExpr :: PureExprDefI t
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

euler1Expr :: PureExprDefI t
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
  ExprIRef.Expression t a ->
  Maybe (ExprIRef.Expression t (Infer.Inferred (DefI t), a), Infer.Context (DefI t))
inferMaybe expr =
  (`runStateT` ctx) $
  Infer.inferLoaded (Infer.InferActions (const Nothing)) loaded node
  where
    (ctx, node) = Infer.initial (Just recursiveDefI)
    loaded = doLoad expr

inferMaybe_ ::
  ExprIRef.Expression t b ->
  Maybe (ExprIRef.Expression t (Infer.Inferred (DefI t)), Infer.Context (DefI t))
inferMaybe_ = (Lens.mapped . Lens._1 . Lens.mapped %~ fst) . inferMaybe

inferAndEncode :: ExprIRef.Expression t a -> Int -> Int
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
