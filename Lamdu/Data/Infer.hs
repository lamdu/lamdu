module Lamdu.Data.Infer
  ( M
  , infer, inferAt
  , unify, unifyRefs
  , Context.freshHole
  -- Re-export:
  , Error(..)
  , Load.LoadedDef
  , Context, emptyContext
  , Scope, RefData.emptyScope, getScope
  , ExprRef
  , TypedValue(..), tvVal, tvType
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.MakeTypes (makeTV)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (Scope(..), LoadedExpr)
import Lamdu.Data.Infer.RefTags (ExprRef)
import Lamdu.Data.Infer.TypedValue (TypedValue(..), tvVal, tvType)
import qualified Control.Lens as Lens
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Expr as Expr
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

type M def = StateT (Context def) (Either (Error def))

getScope :: MonadA m => ExprRef def -> StateT (Context def) m (Scope def)
getScope ref =
  UFData.read ref
  & Lens.zoom Context.ufExprs
  <&> (^. RefData.rdScope)

unify ::
  Ord def =>
  TypedValue def ->
  TypedValue def ->
  M def (TypedValue def)
unify (TypedValue xv xt) (TypedValue yv yt) =
  TypedValue <$> unifyRefs xv yv <*> unifyRefs xt yt

unifyRefs ::
  Ord def => ExprRef def -> ExprRef def ->
  M def (ExprRef def)
unifyRefs x y = InferMRun.run $ Unify.unify x y

freshTV :: (Ord def, MonadA m) => Scope def -> StateT (Context def) m (TypedValue def)
freshTV scope =
  TypedValue <$> Context.freshHole scope <*> Context.freshHole scope

infer ::
  Ord def =>
  Scope def ->
  LoadedExpr def a ->
  M def (LoadedExpr def (TypedValue def, a))
infer scope expr = InferMRun.run $ do
  tv <- InferM.liftContext $ freshTV scope
  exprIntoTV tv expr

inferAt ::
  Ord def =>
  TypedValue def -> LoadedExpr def a ->
  M def (LoadedExpr def (TypedValue def, a))
inferAt tv expr =
  InferMRun.run $ exprIntoTV tv expr

exprIntoTV ::
  Ord def =>
  TypedValue def -> LoadedExpr def a ->
  Infer def (LoadedExpr def (TypedValue def, a))
exprIntoTV dest (Expr.Expr body pl) = do
  scope <-
    InferM.liftContext $
    UFData.read (dest ^. tvVal)
    & Lens.zoom Context.ufExprs
    <&> (^. RefData.rdScope)
  bodyWithTVs <-
    InferM.liftContext $
    case body of
      Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
        paramTypeTV <- freshTV scope
        paramIdRef <- Lens.zoom Context.guidAliases $ GuidAliases.getRep paramGuid
        let
          newScope =
            scope & RefData.scopeMap . Lens.at paramIdRef .~ Just (paramTypeTV ^. tvVal)
        resultWithTV <- addTV newScope result
        let paramTypeWithTV = (paramTypeTV, paramType)
        pure . Expr.BodyLam $ Expr.Lam k paramGuid paramTypeWithTV resultWithTV
      _ -> body & Lens.traverse %%~ addTV scope

  makeTV scope (bodyWithTVs <&> (^. Lens._1)) dest

  bodyResult <- bodyWithTVs & Lens.traverse %%~ uncurry exprIntoTV
  pure $ Expr.Expr bodyResult (dest, pl)
  where
    addTV scope x = do
      tv <- freshTV scope
      return (tv, x)
