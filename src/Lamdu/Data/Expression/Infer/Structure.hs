module Lamdu.Data.Expression.Infer.Structure
  (add) where

import Control.Applicative ((<$), Applicative(..))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.UntilConflict as InferUntilConflict
import qualified Lamdu.Data.Expression.Lens as ExprLens

add ::
  (Ord def, MonadA m) => Infer.Loader def m ->
  Expr.Expression def (Infer.Inferred def, a) ->
  StateT (Infer.Context def) m
  (Expr.Expression def (Infer.Inferred def, a))
add loader expr = do
  withStructure <- expr & Lens.traversed . Lens._1 %%~ addToNode loader
  State.gets $ Infer.derefExpr withStructure

addToNode ::
  (Ord def, MonadA m) => Infer.Loader def m -> Infer.Inferred def ->
  StateT (Infer.Context def) m (Infer.InferNode def)
addToNode loader inferred
  | Lens.has ExprLens.exprHole (Infer.iValue inferred) = do
    loaded <-
      lift . Infer.load loader Nothing . ExprUtil.structureForType . void $
      Infer.iType inferred
    point <$ InferUntilConflict.inferAssertNoConflict "Structure.addToNode" loaded point
  | otherwise = pure point
  where
    point = Infer.iPoint inferred
