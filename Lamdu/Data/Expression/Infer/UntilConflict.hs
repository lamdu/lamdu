module Lamdu.Data.Expression.Infer.UntilConflict
  ( inferUntilConflict, inferAssertNoConflict
  , actions, assertNoConflict
  ) where

import Control.Monad (void)
import Control.Monad.Trans.State (StateT, mapStateT)
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer

inferUntilConflict ::
  Ord def => Infer.Loaded def a -> Infer.Node def ->
  StateT (Infer.Context def) (Either (Infer.Error def))
  (Expr.Expression def (Infer.Inferred def, a))
inferUntilConflict = Infer.inferLoaded actions

actions :: Infer.InferActions def (Either (Infer.Error def))
actions = Infer.InferActions Left

assertNoConflict ::
  Monad m => String -> StateT s (Either (Infer.Error def)) a -> StateT s m a
assertNoConflict msg =
  mapStateT fromEither
  where
    fromEither (Left err) = error . ((msg ++ ":\n") ++) . show $ void err
    fromEither (Right x) = return x

inferAssertNoConflict ::
  (Monad m, Ord def) => String -> Infer.Loaded def a -> Infer.Node def ->
  StateT (Infer.Context def) m (Expr.Expression def (Infer.Inferred def, a))
inferAssertNoConflict msg loaded =
  assertNoConflict msg . inferUntilConflict loaded
