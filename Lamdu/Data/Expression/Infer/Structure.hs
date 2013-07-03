module Lamdu.Data.Expression.Infer.Structure
  (add) where

import Control.Applicative ((<$>), Applicative(..))
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
add loader expr =
  State.gets . Infer.derefExpr =<< addToNodes loader expr

-- TODO: use Lens.outside
addToNodes ::
  (Ord def, MonadA m) =>
  Infer.Loader def m ->
  Expr.Expression def (Infer.Inferred def, a) ->
  StateT (Infer.Context def) m
  (Expr.Expression def (Infer.Node def, a))
addToNodes loader expr@(Expr.Expression body (inferred, a))
  | Lens.has ExprLens.bodyHole body
  && Lens.has ExprLens.exprHole (Infer.iValue inferred)
    = do
    loaded <-
      lift . Infer.load loader Nothing . ExprUtil.structureForType . void $
      Infer.iType inferred
    _ <- InferUntilConflict.inferAssertNoConflict "Structure.addToNode" loaded point
    pure $ expr <&> Lens._1 .~ point
  | otherwise =
    (`Expr.Expression` (point, a)) <$> (body & Lens.traversed %%~ addToNodes loader)
  where
    point = Infer.iNode inferred
