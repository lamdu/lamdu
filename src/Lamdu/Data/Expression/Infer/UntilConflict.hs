module Lamdu.Data.Expression.Infer.UntilConflict
  ( inferUntilConflict, inferAssertNoConflict
  ) where

import Control.Monad (void)
import Control.Monad.Trans.State (StateT, State, mapStateT)
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer

inferUntilConflict ::
  Ord def => Infer.Loaded def a -> Infer.InferNode def ->
  StateT (Infer.Context def) (Either (Infer.Error def))
  (Expr.Expression def (Infer.Inferred def, a))
inferUntilConflict = Infer.inferLoaded actions

actions :: Infer.InferActions def (Either (Infer.Error def))
actions = Infer.InferActions Left

inferAssertNoConflict ::
  Ord def => String -> Infer.Loaded def a -> Infer.InferNode def ->
  State (Infer.Context def) (Expr.Expression def (Infer.Inferred def, a))
inferAssertNoConflict msg loaded =
  mapStateT fromEither . inferUntilConflict loaded
  where
    fromEither (Left err) = error . ((msg ++ ": ") ++) . show $ void err
    fromEither (Right x) = return x
