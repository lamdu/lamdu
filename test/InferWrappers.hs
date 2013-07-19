module InferWrappers where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT, mapStateT, evalStateT)
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

type M = StateT (Infer.Context Def) (Either Error)

load :: Expr.Expression Def a -> M (Expr.Expression (LoadedDef Def) a)
load expr =
  InferLoad.load loader expr
  & mapStateT ((Lens._Left %~ LoadError) . assertSuccess . runEitherT)

assertSuccessM :: (Monad m, Show l) => StateT s (Either l) a -> StateT s m a
assertSuccessM = mapStateT (return . assertSuccess)

assertSuccessT :: (MonadA m, Show l) => StateT s (EitherT l m) a -> StateT s m a
assertSuccessT = mapStateT (fmap assertSuccess . runEitherT)

infer ::
  Expr.Expression (LoadedDef Def) a ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a))
infer expr =
  Infer.infer Infer.emptyScope expr
  & mapStateT (Lens._Left %~ InferError)

deref ::
  Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a) ->
  M (Expr.Expression Def (Derefed Def, a))
deref expr = expr
  & ExprLens.exprDef %~ (^. InferLoad.ldDef)
  & InferDeref.expr

inferDef ::
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a)) ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a))
inferDef act = do
  recursiveDefRef <- InferLoad.newDefinition recursiveDefI
  expr <- act
  _ <-
    mapStateT (Lens._Left %~ InferError) .
    Infer.tempUnify recursiveDefRef $
    expr ^. Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvType
  return expr

runNewContext :: M a -> Either Error a
runNewContext = (`evalStateT` Infer.emptyContext (Random.mkStdGen 0x1337))

-- Weaker and more convenient wrapper around runNewContext, deref,
-- inferDef, infer, load
loadInferDef :: Expr -> Either Error (Expr.Expression Def (Derefed Def))
loadInferDef expr =
  runNewContext $
  (fmap . fmap) fst . deref =<< inferDef (infer =<< load expr)
