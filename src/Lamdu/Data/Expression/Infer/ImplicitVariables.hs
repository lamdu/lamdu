{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Expression.Infer.ImplicitVariables
  ( addVariables, Payload(..)
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
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression.Infer.UntilConflict (inferAssertNoConflict)
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Utils as ExprUtil

data Payload a = Stored a | AutoGen Guid
  deriving (Eq, Ord, Show, Functor, Typeable)
derive makeBinary ''Payload

isUnrestrictedHole :: Expression.Expression def Infer.IsRestrictedPoly -> Bool
isUnrestrictedHole
  (Expression.Expression
    (Expression.BodyLeaf Expression.Hole)
    Infer.UnrestrictedPoly) = True
isUnrestrictedHole _ = False

unMaybe :: StateT s Maybe b -> StateT s Identity b
unMaybe =
  mapStateT (Identity . fromMaybe (error "Infer error when adding implicit vars!"))

-- TODO: Infer.Utils
actions :: Infer.InferActions def Maybe
actions = Infer.InferActions $ const Nothing

addVariableForHole ::
  (Ord def, RandomGen g) =>
  Infer.InferNode def ->
  StateT g (State (Infer.Context def)) (Guid, Infer.InferNode def)
addVariableForHole holePoint = do
  paramGuid <- state random
  let
    getVar = ExprUtil.pureExpression $ Lens.review ExprUtil.bodyParameterRef paramGuid
    loaded =
      fromMaybe (error "Should not be loading defs when loading a mere getVar") $
      Infer.load loader Nothing getVar
  lift $ do
    inferredGetVar <-
      inferAssertNoConflict "ImplicitVariables.addVariableForHole" loaded holePoint
    let
      paramTypeRef =
        Infer.tvType . Infer.nRefs . Infer.iPoint . fst $
        inferredGetVar ^. Expression.ePayload
    paramTypeTypeRef <- Infer.createRefExpr
    return
      ( paramGuid
      , Infer.InferNode (Infer.TypedValue paramTypeRef paramTypeTypeRef) mempty
      )
  where
    loader = Infer.Loader $ const Nothing

addVariablesForExpr ::
  (MonadA m, Ord def, RandomGen g) =>
  Infer.Loader def m ->
  Expression.Expression def (Infer.Inferred def, a) ->
  StateT g (StateT (Infer.Context def) m) [(Guid, Infer.InferNode def)]
addVariablesForExpr loader expr = do
  reinferred <-
    lift . State.gets . Infer.derefExpr $
    expr & Lens.traversed . Lens._1 %~ Infer.iPoint
  if isUnrestrictedHole $ inferredVal reinferred
    then
      fmap (:[]) . mapStateT toStateT . addVariableForHole $
      Infer.iPoint . fst $ expr ^. Expression.ePayload
    else do
      reloaded <-
        lift . lift . Infer.load loader Nothing $ -- <-- TODO: Nothing?
        inferredVal reinferred
      reinferredLoaded <-
        lift . toStateT .
        inferAssertNoConflict "ImplicitVariables.addVariableForExpr"
        reloaded .
        Infer.iPoint . fst $ Lens.view Expression.ePayload reinferred
      fmap concat . mapM (addVariablesForExpr loader) .
        filter (isUnrestrictedHole . inferredVal) $
        ExprUtil.subExpressions reinferredLoaded
  where
    inferredVal = Infer.iValue . fst . Lens.view Expression.ePayload

addParam ::
  Ord def =>
  Expression.Expression def (Infer.InferNode def, Payload a) ->
  (Guid, Infer.InferNode def) ->
  State (Infer.Context def)
  (Expression.Expression def (Infer.InferNode def, Payload a))
addParam body (paramGuid, paramTypeNode) = do
  newRootNode <- Infer.newNodeWithScope mempty
  let
    newRootExpr =
      Expression.Expression newRootLam (newRootNode, AutoGen (Guid.augment "root" paramGuid))
  unMaybe $ Infer.addRules actions [fst <$> newRootExpr]
  return newRootExpr
  where
    paramTypeExpr =
      Expression.Expression
      (Expression.BodyLeaf Expression.Hole)
      (paramTypeNode, AutoGen (Guid.augment "paramType" paramGuid))
    newRootLam =
      ExprUtil.makeLambda paramGuid paramTypeExpr body

addVariables ::
  (MonadA m, Ord def, RandomGen g) =>
  g -> Infer.Loader def m ->
  Expression.Expression def (Infer.Inferred def, a) ->
  StateT (Infer.Context def) m
  (Expression.Expression def (Infer.Inferred def, Payload a))
addVariables gen loader expr = do
  implicitParams <-
    (`evalStateT` gen) . fmap concat .
    mapM (addVariablesForExpr loader) $ ExprUtil.funcArguments expr
  newRoot <- toStateT $ foldM addParam baseExpr implicitParams
  State.gets $ Infer.derefExpr newRoot
  where
    baseExpr =
      expr & Lens.traversed %~
      (Lens._1 %~ Infer.iPoint) .
      (Lens._2 %~ Stored)
