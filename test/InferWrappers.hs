module InferWrappers where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT, mapStateT, runStateT)
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
  InferLoad.Loader loadDefType
  where
    loadDefType key =
      case Map.lookup key definitionTypes of
      Nothing -> Left ("Could not find" ++ show key)
      Just x -> Right x

assertSuccess :: Show err => Either err a -> a
assertSuccess = either (error . show) id

data Error = InferError (Infer.Error Def) | LoadError (InferLoad.Error Def)
  deriving (Show)

load ::
  Expr.Expression Def a ->
  StateT (Infer.Context Def) (Either Error)
  (Expr.Expression (LoadedDef Def) a)
load expr =
  InferLoad.load loader expr
  & mapStateT ((Lens._Left %~ LoadError) . assertSuccess . runEitherT)

assertSuccessM :: (Monad m, Show l) => StateT s (Either l) a -> StateT s m a
assertSuccessM = mapStateT (return . assertSuccess)

assertSuccessT :: (MonadA m, Show l) => StateT s (EitherT l m) a -> StateT s m a
assertSuccessT = mapStateT (fmap assertSuccess . runEitherT)

infer ::
  Expr.Expression (LoadedDef Def) a ->
  StateT
  (Infer.Context Def)
  (Either Error)
  (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a))
infer expr =
  Infer.infer Infer.emptyScope expr
  & mapStateT (Lens._Left %~ InferError)

runContext ::
  StateT (Infer.Context Def) (Either Error)
  (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, ())) ->
  Either Error
  (Expr.Expression Def (Derefed Def), Infer.Context Def)
runContext act =
  (`runStateT` Infer.emptyContext (Random.mkStdGen 0x1337)) $ do
    recursiveDefRef <- InferLoad.newDefinition recursiveDefI
    inferredExpr <- act
    let
      inferredExprTypeRef =
        inferredExpr ^. Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvType
    _ <- mapStateT (Lens._Left %~ InferError) $ Infer.unify recursiveDefRef inferredExprTypeRef
    inferredExpr
      & ExprLens.exprDef %~ (^. InferLoad.ldDef)
      & InferDeref.expr
      <&> Lens.mapped %~ fst

loadInferRun :: Expr -> Either Error (Expr.Expression Def (Derefed Def), Infer.Context Def)
loadInferRun expr = runContext $ load expr >>= infer

loadInferRunAssertSuccess :: Expr -> (Expr.Expression Def (Derefed Def), Infer.Context Def)
loadInferRunAssertSuccess =
  assertSuccess . loadInferRun
