{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Expression.Infer.ImplicitVariables
  ( add, Payload(..)
  ) where

import Control.Applicative ((<$>))
import Control.Lens ((^.), (%~), (&))
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, State, evalStateT, mapStateT, state)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.UntilConflict as InferUntilConflict
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

data Payload a = Stored a | AutoGen Guid
  deriving (Eq, Ord, Show, Functor, Typeable)
derive makeBinary ''Payload

isUnrestrictedHole :: Expr.Expression def Infer.IsRestrictedPoly -> Bool
isUnrestrictedHole
  (Expr.Expression
    (Expr.BodyLeaf Expr.Hole)
    Infer.UnrestrictedPoly) = True
isUnrestrictedHole _ = False

addVariableForHole ::
  (Show def, Ord def, RandomGen g) =>
  Infer.InferNode def ->
  StateT g (State (Infer.Context def)) (Guid, Infer.InferNode def)
addVariableForHole holePoint = do
  paramGuid <- state random
  let
    getVar = ExprUtil.pureExpression $ Lens.review ExprLens.bodyParameterRef paramGuid
    loaded = Infer.loadIndependent (("Loading a mere getVar: " ++) . show) Nothing getVar
  lift $ do
    inferredGetVar <-
      InferUntilConflict.inferAssertNoConflict
      "ImplicitVariables.addVariableForHole" loaded holePoint
    let
      paramTypeRef =
        Infer.tvType . Infer.nRefs . Infer.iPoint . fst $
        inferredGetVar ^. Expr.ePayload
    paramTypeTypeRef <- Infer.createRefExpr
    return
      ( paramGuid
      , Infer.InferNode (Infer.TypedValue paramTypeRef paramTypeTypeRef) mempty
      )

addVariablesForExpr ::
  (MonadA m, Show def, Ord def, RandomGen g) =>
  Infer.Loader def m ->
  Expr.Expression def (Infer.Inferred def, a) ->
  StateT g (StateT (Infer.Context def) m) [(Guid, Infer.InferNode def)]
addVariablesForExpr loader expr = do
  reinferred <-
    lift . State.gets . Infer.derefExpr $
    expr & Lens.traversed . Lens._1 %~ Infer.iPoint
  if isUnrestrictedHole $ inferredVal reinferred
    then
      fmap (:[]) . mapStateT toStateT . addVariableForHole $
      Infer.iPoint . fst $ expr ^. Expr.ePayload
    else do
      reloaded <-
        lift . lift . Infer.load loader Nothing $ -- <-- TODO: Nothing?
        inferredVal reinferred
      reinferredLoaded <-
        lift . toStateT .
        InferUntilConflict.inferAssertNoConflict
        "ImplicitVariables.addVariableForExpr" reloaded .
        Infer.iPoint . fst $ Lens.view Expr.ePayload reinferred
      fmap concat . mapM (addVariablesForExpr loader) .
        filter (isUnrestrictedHole . inferredVal) $
        ExprUtil.subExpressionsWithoutTags reinferredLoaded
  where
    inferredVal = Infer.iValue . fst . Lens.view Expr.ePayload

addParam ::
  Ord def =>
  Expr.Expression def (Infer.InferNode def, Payload a) ->
  (Guid, Infer.InferNode def) ->
  State (Infer.Context def)
  (Expr.Expression def (Infer.InferNode def, Payload a))
addParam body (paramGuid, paramTypeNode) = do
  newRootNode <- Infer.newNodeWithScope mempty
  let
    newRootExpr =
      Expr.Expression newRootLam (newRootNode, AutoGen (Guid.augment "root" paramGuid))
  InferUntilConflict.assertNoConflict "Infer error when adding implicit vars" $
    Infer.addRules InferUntilConflict.actions [fst <$> newRootExpr]
  return newRootExpr
  where
    paramTypeExpr =
      Expr.Expression
      (Expr.BodyLeaf Expr.Hole)
      (paramTypeNode, AutoGen (Guid.augment "paramType" paramGuid))
    newRootLam =
      ExprUtil.makeLambda paramGuid paramTypeExpr body

add ::
  (MonadA m, Ord def, Show def, RandomGen g) =>
  g -> Infer.Loader def m ->
  Expr.Expression def (Infer.Inferred def, a) ->
  StateT (Infer.Context def) m
  (Expr.Expression def (Infer.Inferred def, Payload a))
add gen loader expr = do
  implicitParams <-
    (`evalStateT` gen) . fmap concat .
    mapM (addVariablesForExpr loader) $ ExprUtil.curriedFuncArguments expr
  newRoot <- toStateT $ foldM addParam baseExpr implicitParams
  State.gets $ Infer.derefExpr newRoot
  where
    baseExpr =
      expr & Lens.traversed %~
      (Lens._1 %~ Infer.iPoint) .
      (Lens._2 %~ Stored)
