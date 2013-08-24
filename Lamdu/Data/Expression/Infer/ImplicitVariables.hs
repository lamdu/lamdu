{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Expression.Infer.ImplicitVariables
  ( add, Payload(..)
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
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
  Infer.Node def ->
  StateT g (State (Infer.Context def)) (Guid, Infer.Node def)
addVariableForHole holePoint = do
  paramGuid <- state random
  let
    getVar = ExprLens.pureExpr . ExprLens.bodyParameterRef # paramGuid
    getVarLoaded = Infer.loadIndependent (("Loading a mere getVar: " ++) . show) Nothing getVar
  lift $ do
    inferredGetVar <-
      InferUntilConflict.inferAssertNoConflict
      "ImplicitVariables.addVariableForHole" getVarLoaded holePoint
    let
      paramTypeRef =
        Infer.tvType . Infer.nRefs . Infer.iNode . fst $
        inferredGetVar ^. Expr.ePayload
    paramTypeTypeRef <- Infer.createRefExpr
    return
      ( paramGuid
      , Infer.Node (Infer.TypedValue paramTypeRef paramTypeTypeRef) mempty
      )

addVariablesForExpr ::
  (MonadA m, Show def, Ord def, RandomGen g) =>
  Infer.Loader def m ->
  Expr.Expression def (Infer.Inferred def, a) ->
  StateT g (StateT (Infer.Context def) m) [(Guid, Infer.Node def)]
addVariablesForExpr loader expr = do
  reinferred <-
    lift . State.gets . Infer.derefExpr $
    expr & Lens.traversed . Lens._1 %~ Infer.iNode
  if isUnrestrictedHole $ inferredVal reinferred
    then
      fmap (:[]) . mapStateT toStateT . addVariableForHole $
      Infer.iNode . fst $ expr ^. Expr.ePayload
    else do
      reloaded <-
        lift . lift . Infer.load loader Nothing $ -- <-- TODO: Nothing?
        inferredVal reinferred
      reinferredLoaded <-
        lift . toStateT .
        InferUntilConflict.inferAssertNoConflict
        "ImplicitVariables.addVariableForExpr" reloaded .
        Infer.iNode $ reinferred ^. Expr.ePayload . Lens._1
      fmap concat . mapM (addVariablesForExpr loader) .
        filter (isUnrestrictedHole . inferredVal) $
        ExprUtil.subExpressionsWithout ExprLens.tagPositions reinferredLoaded
  where
    inferredVal = Infer.iValue . fst . (^. Expr.ePayload)

lambdaWrap ::
  Ord def => Guid -> Infer.Node def ->
  Expr.Expression def (Infer.Node def, a) ->
  a -> a ->
  State (Infer.Context def)
  (Expr.Expression def (Infer.Node def, a))
lambdaWrap paramGuid paramTypeNode result lamPl paramTypePl = do
  newNode <- Infer.newNodeWithScope mempty
  let newExpr = Expr.Expression newLam (newNode, lamPl)
  InferUntilConflict.assertNoConflict "Infer error when adding implicit vars" $
    Infer.addRules InferUntilConflict.actions [fst <$> newExpr]
  return newExpr
  where
    newLam =
      ExprUtil.makeLambda paramGuid paramTypeExpr result
    paramTypeExpr =
      Expr.Expression
      (Expr.BodyLeaf Expr.Hole)
      (paramTypeNode, paramTypePl)

addParam ::
  Ord def =>
  Expr.Expression def (Infer.Node def, Payload a) ->
  (Guid, Infer.Node def) ->
  State (Infer.Context def)
  (Expr.Expression def (Infer.Node def, Payload a))
addParam result (paramGuid, paramTypeNode) =
  lambdaWrap paramGuid paramTypeNode result lamPl paramTypePl
  where
    lamPl = AutoGen $ Guid.augment "" paramGuid
    paramTypePl = AutoGen $ Guid.augment "paramType" paramGuid

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
      (Lens._1 %~ Infer.iNode) .
      (Lens._2 %~ Stored)
