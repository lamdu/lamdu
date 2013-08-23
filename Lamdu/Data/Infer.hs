module Lamdu.Data.Infer
  ( infer, unify, unifyRefs, freshHole
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
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.MakeTypes (makeTV)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (Scope(..))
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.TypedValue (TypedValue(..), tvVal, tvType, ScopedTypedValue(..), stvTV, stvScope)
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Load as Load
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Monad.Run as InferMRun
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.Unify as Unify
import qualified System.Random as Random

-- Renamed for export purposes
emptyContext :: Random.StdGen -> Context def
emptyContext = Context.empty

freshHole :: MonadA m => Scope def -> StateT (Context def) m (ExprRef def)
freshHole = Lens.zoom Context.uFExprs . RefData.freshHole

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
unifyRefs x y = InferMRun.run $ Unify.unify x y

infer ::
  Ord def => Scope def -> Expr.Expression (Load.LoadedDef def) a ->
  StateT (Context def) (Either (Error def))
  (Expr.Expression (Load.LoadedDef def) (ScopedTypedValue def, a))
infer scope expr = InferMRun.run $ exprIntoSTV scope expr

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
