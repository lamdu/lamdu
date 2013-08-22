module Lamdu.Data.Infer
  ( infer, unify, unifyRefs, freshHole, lambdaWrap
  -- Re-export:
  , Error(..)
  , Load.LoadedDef
  , Context, emptyContext
  , Scope, RefData.emptyScope
  , ExprRef
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.Load (LoadedDef)
import Lamdu.Data.Infer.MakeTypes (makeTV)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (Scope(..))
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.TypedValue (TypedValue(..), tvVal, tvType, ScopedTypedValue(..), stvTV, stvScope)
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.LamWrap as LamWrap
import qualified Lamdu.Data.Infer.Load as Load
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.Rule as Rule
import qualified Lamdu.Data.Infer.Unify as Unify
import qualified System.Random as Random

-- Renamed for export purposes
emptyContext :: Random.StdGen -> Context def
emptyContext = Context.empty

freshHole :: MonadA m => Scope def -> StateT (Context def) m (ExprRef def)
freshHole = Lens.zoom Context.uFExprs . RefData.freshHole

lambdaWrap ::
  Ord def =>
  Guid -> ExprRef def ->
  Expr.Expression (LoadedDef def) (ScopedTypedValue def, a) ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (LoadedDef def) (ScopedTypedValue def, Maybe a))
lambdaWrap =
  LamWrap.lambdaWrap
  & Lens.mapped . Lens.mapped . Lens.mapped %~ runInfer

unify ::
  Ord def =>
  TypedValue def ->
  TypedValue def ->
  StateT (Context def) (Either (Error def)) (TypedValue def)
unify (TypedValue xv xt) (TypedValue yv yt) =
  TypedValue <$> unifyRefs xv yv <*> unifyRefs xt yt

unifyRefs ::
  Ord def => ExprRef def -> ExprRef def ->
  StateT (Context def) (Either (Error def)) (ExprRef def)
unifyRefs x y = runInfer $ Unify.unify x y

infer ::
  Ord def => Scope def -> Expr.Expression (Load.LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (Load.LoadedDef def) (ScopedTypedValue def, a))
infer scope expr = runInfer $ exprIntoSTV scope expr

runInfer :: Ord def => Infer def a -> StateT (Context def) (Either (Error def)) a
runInfer act = do
  (res, rulesTriggered) <- runInferWriter act
  go rulesTriggered
  return res
  where
    runInferWriter = runWriterT . (^. Lens.from InferM.infer)
    go (InferM.TriggeredRules oldRuleRefs) =
      case OR.refMapMinViewWithKey oldRuleRefs of
      Nothing -> return ()
      Just ((firstRuleRef, triggers), ruleIds) ->
        go . filterRemovedRule firstRuleRef . (Lens._2 <>~ InferM.TriggeredRules ruleIds) =<<
        runInferWriter (Rule.execute firstRuleRef triggers)
    filterRemovedRule _ (True, rules) = rules
    filterRemovedRule ruleId (False, InferM.TriggeredRules rules) =
      InferM.TriggeredRules $ rules & Lens.at ruleId .~ Nothing

-- With hole apply vals and hole types
exprIntoSTV ::
  Ord def => Scope def -> Expr.Expression (Load.LoadedDef def) a ->
  Infer def (Expr.Expression (Load.LoadedDef def) (ScopedTypedValue def, a))
exprIntoSTV scope (Expr.Expression body pl) = do
  bodySTV <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      paramIdRef <- InferM.liftGuidAliases $ GuidAliases.getRep paramGuid
      let
        newScope =
          scope
          & RefData.scopeMap . Lens.at paramIdRef .~ Just
            (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      pure . Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
    _ ->
      body & Lens.traverse %%~ exprIntoSTV scope
  tv <- bodySTV <&> (^. Expr.ePayload . Lens._1) & makeTV scope
  pure $
    Expr.Expression bodySTV
    (ScopedTypedValue tv scope, pl)
