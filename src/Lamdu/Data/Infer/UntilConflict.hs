module Lamdu.Data.Infer.UntilConflict
  ( inferUntilConflict, inferAssertNoConflict
  ) where

import Control.Monad (void)
import Control.Monad.Trans.State (StateT, State, mapStateT)
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Infer as Infer

inferUntilConflict ::
  Ord def => Infer.Loaded def a -> Infer.InferNode def ->
  StateT (Infer.Context def) (Either (Infer.Error def))
  (Expression.Expression def (Infer.Inferred def, a))
inferUntilConflict = Infer.inferLoaded actions

actions :: Infer.InferActions def (Either (Infer.Error def))
actions = Infer.InferActions Left

inferAssertNoConflict ::
  Ord def => Infer.Loaded def a -> Infer.InferNode def ->
  State (Infer.Context def) (Expression.Expression def (Infer.Inferred def, a))
inferAssertNoConflict loaded =
  mapStateT fromEither . inferUntilConflict loaded
  where
    fromEither (Left err) = error . show $ void err
    fromEither (Right x) = return x
