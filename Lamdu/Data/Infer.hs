module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify
  , emptyContext
  , exprSTVRefs
  -- Re-export:
  , Context
  , Scope, emptyScope
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  , Optimize.optimizeContext
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (StateT)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.AppliedPiResult (handleAppliedPiResult)
import Lamdu.Data.Infer.GetField (handleGetField)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.MakeTypes (makeTypeRef)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Optimize as Optimize
import qualified Lamdu.Data.Infer.Unify as Unify

unify ::
  Eq def =>
  TypedValue ->
  TypedValue ->
  StateT (Context def) (Either (Error def)) ()
unify (TypedValue xv xt) (TypedValue yv yt) = do
  void . runInfer $ Unify.unify xv yv
  void . runInfer $ Unify.unify xt yt

exprSTVRefs :: Lens.Traversal' (Expr.Expression (LoadedDef def) (ScopedTypedValue, a)) Ref
exprSTVRefs f = ExprLens.exprBitraverse (ldType f) ((Lens._1 . stvRefs) f)

infer ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer scope expr = runInfer $ exprIntoSTV scope expr

isTag :: Ref -> Infer def ()
isTag ref = do
  refData <- ExprRefs.read ref
  case refData ^. rdIsComposite of
    Monoid.Any True -> InferM.error $ CompositeTag ref
    _ -> return ()

executeRelation :: Eq def => Relation -> Ref -> Infer def ()
executeRelation rel =
  case rel of
  RelationAppliedPiResult apr -> flip handleAppliedPiResult apr
  RelationGetField getField -> const (handleGetField getField)
  RelationIsTag -> isTag

runInfer :: Eq def => Infer def a -> StateT (Context def) (Either (Error def)) a
runInfer = InferM.run $ InferM.InferActions executeRelation

-- With hole apply vals and hole types
exprIntoSTV ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body pl) = do
  bodySTV <-
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
    bodySTV
    & ExprLens.bodyDef %~ (^. ldDef)
    & mkRefData
    & ExprRefs.fresh
  typeRef <-
    bodySTV <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression bodySTV
    (ScopedTypedValue (TypedValue valRef typeRef) scope, pl)
  where
    mkRefData bodySTV
      | shouldCircumsize bodySTV =
        defaultRefData scope (ExprLens.bodyHole # ())
        & rdIsComposite .~ Monoid.Any True
      | otherwise = defaultRefData scope $ bodySTV <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    shouldCircumsize (Expr.BodyApply (Expr.Apply func _))
      | Lens.nullOf ExprLens.exprDefinitionRef func = True
    shouldCircumsize (Expr.BodyGetField (Expr.GetField record _))
      | Lens.nullOf ExprLens.exprDefinitionRef record = True
    shouldCircumsize _ = False
