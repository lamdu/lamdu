module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify, tempUnify
  , emptyContext
  -- Re-export:
  , Context
  , Scope, emptyScope
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (StateT)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.MakeTypes (makeTypeRef)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.Subst (substOrUnify)
import Lamdu.Data.Infer.Unify (unify)
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

-- | Temporarily here for tests until we fix API to only have
-- TypedValue unify:
tempUnify :: Eq def => Ref -> Ref -> StateT (Context def) (Either (Error def)) ()
tempUnify x y = unify x y & runInfer & void

infer ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope expr = exprIntoSTV scope expr & runInfer

runInfer :: Eq def => Infer def a -> StateT (Context def) (Either (Error def)) a
runInfer = InferM.run $ InferM.InferActions substOrUnify

-- With hole apply vals and hole types
exprIntoSTV ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body pl) = do
  newBody <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      let
        newScope =
          scope
          & scopeMap . Lens.at paramGuid .~
            Just (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      pure . Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
    _ ->
      body & Lens.traverse %%~ exprIntoSTV scope
  valRef <-
    newBody
    & circumcizeApply
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & defaultRefData scope
    & ExprRefs.fresh
  typeRef <-
    newBody <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression newBody
    (ScopedTypedValue (TypedValue valRef typeRef) scope, pl)
  where
    -- Except of apply of type constructors:
    circumcizeApply x
      | Lens.nullOf Expr._BodyApply x = x
      | Lens.has (Expr._BodyApply . Expr.applyFunc . ExprLens.exprDefinitionRef) x = x
      | otherwise = ExprLens.bodyHole # ()
