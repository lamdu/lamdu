module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify
  , emptyContext
  , exprSTVRefs
  -- Re-export:
  , Context
  , Scope, emptyScope
  , Ref
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  , Optimize.optimizeContext
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT)
import Data.OpaqueRef (Ref)
import Lamdu.Data.Infer.AppliedPiResult (handleAppliedPiResult)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.MakeTypes (makeTypeRef)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import qualified Control.Lens as Lens
import qualified Data.IntMap as IntMap
import qualified Data.Monoid as Monoid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Optimize as Optimize
import qualified Lamdu.Data.Infer.Rule as Rule
import qualified Lamdu.Data.Infer.Unify as Unify

unify ::
  Eq def =>
  TypedValue def ->
  TypedValue def ->
  StateT (Context def) (Either (Error def)) ()
unify (TypedValue xv xt) (TypedValue yv yt) = do
  void . runInfer $ Unify.unify xv yv
  void . runInfer $ Unify.unify xt yt

exprSTVRefs :: Lens.Traversal' (Expr.Expression (LoadedDef def) (ScopedTypedValue def, a)) (RefD def)
exprSTVRefs f = ExprLens.exprBitraverse (ldType f) ((Lens._1 . stvRefs) f)

infer ::
  Eq def => Scope def -> Expr.Expression (LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (LoadedDef def) (ScopedTypedValue def, a))
infer scope expr = runInfer $ exprIntoSTV scope expr

executeRelation :: Eq def => Relation def -> RefD def -> Infer def ()
executeRelation rel =
  case rel of
  RelationAppliedPiResult apr -> flip handleAppliedPiResult apr

runInfer :: Eq def => Infer def a -> StateT (Context def) (Either (Error def)) a
runInfer act = do
  (res, rulesTriggered) <- runWriterT $ inferToWriter act
  go rulesTriggered
  return res
  where
    inferToWriter = InferM.run (InferM.InferActions executeRelation)
    go (InferM.TriggeredRules oldRuleIds) =
      case IntMap.minViewWithKey oldRuleIds of
      Nothing -> return ()
      Just ((firstRuleId, triggers), ruleIds) ->
        go . filterRemovedRule firstRuleId . (Lens._2 <>~ InferM.TriggeredRules ruleIds) =<<
        (runWriterT . inferToWriter)
        (Rule.execute firstRuleId triggers)
    filterRemovedRule _ (True, rules) = rules
    filterRemovedRule ruleId (False, InferM.TriggeredRules rules) =
      InferM.TriggeredRules $ IntMap.delete ruleId rules

-- With hole apply vals and hole types
exprIntoSTV ::
  Eq def => Scope def -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue def, a))
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
    & InferM.liftExprRefs . ExprRefs.fresh
  typeRef <-
    bodySTV <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression bodySTV
    (ScopedTypedValue (TypedValue valRef typeRef) scope, pl)
  where
    mkRefData bodySTV
      | shouldCircumsize bodySTV =
        defaultRefData scope (ExprLens.bodyHole # ())
        & rdIsCircumsized .~ Monoid.Any True
      | otherwise = defaultRefData scope $ bodySTV <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    shouldCircumsize (Expr.BodyApply (Expr.Apply func _))
      | Lens.nullOf ExprLens.exprDefinitionRef func = True
    shouldCircumsize (Expr.BodyGetField (Expr.GetField record _))
      | Lens.nullOf ExprLens.exprDefinitionRef record = True
    shouldCircumsize _ = False
