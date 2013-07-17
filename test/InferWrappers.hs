module InferWrappers where

import Control.Lens.Operators
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (State, mapStateT, runState)
import Data.Monoid (Monoid(..))
import Lamdu.Data.Infer.Deref (Derefed(..))
import Lamdu.Data.Infer.Load (LoadedDef)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Deref as InferDeref
import qualified Lamdu.Data.Infer.Load as InferLoad
import qualified System.Random as Random

type ExprInferred = Expr.Expression Def ( Expr, Expr )

inferResults :: Expr.Expression Def (Derefed Def) -> ExprInferred
inferResults = fmap $ \(Derefed val typ) -> (val, typ)

loader :: InferLoad.Loader Def (Either String)
loader =
  InferLoad.Loader load
  where
    load key =
      case Map.lookup key definitionTypes of
      Nothing -> Left ("Could not find" ++ show key)
      Just x -> Right x

unEither :: Show err => Either err a -> a
unEither = either (error . show) id

ridEither :: (Show err, Monad m) => Either err a -> m a
ridEither = return . unEither

ridEitherT :: (Functor m, Show err) => EitherT err m a -> m a
ridEitherT = fmap unEither . runEitherT

doLoad ::
  Expr.Expression Def a ->
  State (Infer.Context Def)
  (Expr.Expression (LoadedDef Def) a)
doLoad = mapStateT (ridEither . ridEitherT) . InferLoad.load loader

doInfer ::
  Expr.Expression (LoadedDef Def) a ->
  State (Infer.Context Def)
  (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a))
doInfer = mapStateT ridEither . Infer.infer mempty

doLoadInfer ::
  Expr.Expression Def a ->
  State (Infer.Context Def)
  (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a))
doLoadInfer expr =
  doLoad expr >>= doInfer

runContext ::
  State (Infer.Context Def) (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, ())) ->
  (Expr.Expression Def (Derefed Def), Infer.Context Def)
runContext act =
  (`runState` Infer.emptyContext (Random.mkStdGen 0x1337)) $ do
    recursiveDefRef <- InferLoad.newDefinition recursiveDefI
    inferredExpr <- act
    let
      inferredExprTypeRef =
        inferredExpr ^. Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvType
    mapStateT ridEither $ Infer.unify recursiveDefRef inferredExprTypeRef
    inferredExpr
      & ExprLens.exprDef %~ (^. InferLoad.ldDef)
      & InferDeref.expr
      <&> Lens.mapped %~ fst

doLoadInferRun :: PureExpr Def -> (Expr.Expression Def (Derefed Def), Infer.Context Def)
doLoadInferRun = runContext . doLoadInfer
