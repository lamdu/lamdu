module Lamdu.Data.Infer.Structure
  ( add
  ) where

import Control.Lens.Operators
import Control.Monad (when, void)
import Control.Monad.Trans.State (StateT, mapStateT)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.TypedValue (TypedValue(..))
import qualified Control.Lens as Lens
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.Deref as Deref
import qualified Lamdu.Data.Infer.Load as Load
import qualified Lamdu.Data.Infer.RefData as RefData

add ::
  (Show def, Ord def) =>
  Expr.Expression (Load.LoadedDef def) (TypedValue def, a) ->
  StateT (Context def) (Either (Deref.Error def)) ()
add = Lens.traverseOf_ (ExprLens.holePayloads . Lens._1) %%~ addToHole

addToHole ::
  (Show def, Ord def) =>
  TypedValue def -> StateT (Context def) (Either (Deref.Error def)) ()
addToHole (TypedValue valRef typRef) = do
  scope <-
    UFData.read valRef
    & Lens.zoom Context.ufExprs
    <&> (^. RefData.rdScope)
  valData <- Lens.zoom Context.ufExprs $ UFData.read valRef
  when (Lens.has (RefData.rdBody . ExprLens.bodyHole) valData) $ do
    -- Both the "stored" and inferred val are holes, time to fill some structure:
    -- NOTE: safe to use 'deref []' because we don't care about the guids at all.
    typExpr <- Deref.deref [] typRef <&> void
    structureRef <- Load.exprIntoContext scope (ExprUtil.structureForType typExpr)
    _ <- mapStateT assertSuccess $ Infer.unifyRefs valRef structureRef
    return ()
  where
    assertSuccess (Right x) = Right x
    assertSuccess (Left err) =
      error $ "addToHole, unify of hole and inferred structure failed: " ++ show err
