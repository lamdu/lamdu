module InferWrappers where

import Control.Lens.Operators
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (State, mapStateT, evalState)
import Lamdu.Data.Infer (Infer)
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

doLoad :: PureExpr Def -> State (Infer.Context Def) (PureExpr (LoadedDef Def))
doLoad = mapStateT (ridEither . ridEitherT) . InferLoad.load loader

runContext ::
  Infer Def (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, ())) ->
  ExprInferred
runContext act =
  inferResults . (`evalState` Infer.emptyContext) $ do
    inferredExpr <- mapStateT ridEither $ do
      recursiveDefRef <- InferLoad.newDefinition recursiveDefI
      inferredExpr <- act
      let
        inferredExprTypeRef =
          inferredExpr ^. Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvType
      Infer.unify recursiveDefRef inferredExprTypeRef
      return inferredExpr
    inferredExpr
      & ExprLens.exprDef %~ (^. InferLoad.ldDef)
      & InferDeref.expr
      <&> Lens.mapped %~ fst
      & mapStateT ridEither
