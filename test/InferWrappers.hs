module InferWrappers where

import AnnotatedExpr
import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (State, StateT(..))
import Data.Binary.Utils (encodeS)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import Utils
import qualified Control.Lens as Lens
import qualified Data.ByteString as SBS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer


type InferResults t =
  ExprIRef.Expression t
  ( PureExprDefI t
  , PureExprDefI t
  )

inferResults :: ExprIRef.Expression t (Infer.Inferred (DefI t)) -> InferResults t
inferResults = fmap (void . Infer.iValue &&& void . Infer.iType)

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

showExpressionWithConflicts ::
  ExprIRef.Expression t (InferredWithConflicts (DefI t)) -> String
showExpressionWithConflicts =
  snd . errorMessage . traverse annotateIWC
  where
    annotateIWC (InferredWithConflicts inferred tErrors vErrors) =
      concat <$>
      sequence
      [ check "val:" (Infer.iValue inferred) vErrors
      , check "type:" (Infer.iType inferred) tErrors
      ]
    check _ _ [] = pure []
    check str iexpr conflicts =
      fmap (: []) . addAnnotation . List.intercalate "\n" $
      [ "Inferred " ++ str
      , "  " ++ showInferred iexpr
      , "Conflicts:"
      ] ++
      map (("  "++) . show . Infer.errDetails) conflicts

doInferM ::
  Infer.Node (DefI t) -> ExprIRef.Expression t a ->
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

loadInferM ::
  Infer.Node (DefI t) -> ExprIRef.Expression t a ->
  StateT (Infer.Context (DefI t)) (Either (Infer.Error (DefI t)))
  (ExprIRef.Expression t (Infer.Inferred (DefI t), a))
loadInferM inferNode expr =
  Infer.inferLoaded eitherActions (doLoad expr) inferNode
  where
    eitherActions = Infer.InferActions Left

loadInfer ::
  ExprIRef.Expression t a ->
  Either (Infer.Error (DefI t))
  (ExprIRef.Expression t (Infer.Inferred (DefI t), a), Infer.Context (DefI t))
loadInfer expr = fromInitialState (`loadInferM` expr)

loadInferResults ::
  ExprIRef.Expression t () ->
  Either (Infer.Error (DefI t)) (InferResults t)
loadInferResults = fmap (inferResults . fmap fst . fst) . loadInfer

fromInitialState ::
  (Infer.Node (DefI t) -> StateT (Infer.Context (DefI t)) m a) -> m (a, Infer.Context (DefI t))
fromInitialState f =
  (`runStateT` ctx) $ f node
  where
    (ctx, node) = Infer.initial $ Just recursiveDefI

doInferM_ ::
  Infer.Node (DefI t) -> ExprIRef.Expression t a ->
  State (Infer.Context (DefI t)) (ExprIRef.Expression t (Infer.Inferred (DefI t)))
doInferM_ = (fmap . fmap . fmap . fmap) fst doInferM

doInfer ::
  ExprIRef.Expression t a ->
  ( ExprIRef.Expression t (Infer.Inferred (DefI t), a)
  , Infer.Context (DefI t)
  )
doInfer expr = runIdentity $ fromInitialState (`doInferM` expr)

doInfer_ ::
  ExprIRef.Expression t a ->
  (ExprIRef.Expression t (Infer.Inferred (DefI t)), Infer.Context (DefI t))
doInfer_ = (Lens._1 . Lens.mapped %~ fst) . doInfer

inferMaybe ::
  ExprIRef.Expression t a ->
  Maybe (ExprIRef.Expression t (Infer.Inferred (DefI t), a), Infer.Context (DefI t))
inferMaybe expr =
  fromInitialState $ Infer.inferLoaded maybeActions (doLoad expr)
  where
    maybeActions = Infer.InferActions $ const Nothing

inferMaybe_ ::
  ExprIRef.Expression t b ->
  Maybe (ExprIRef.Expression t (Infer.Inferred (DefI t)), Infer.Context (DefI t))
inferMaybe_ = (Lens.mapped . Lens._1 . Lens.mapped %~ fst) . inferMaybe

inferAndEncode :: ExprIRef.Expression t a -> Int -> Int
inferAndEncode expr par = SBS.length . encodeS $ fst result
  where
    result =
      fromMaybe (error "Conflicts in infer") .
      inferMaybe_ . void $
      fmap (const par) expr
