module InferWrappers where

import Control.Lens.Operators
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT, mapStateT, evalStateT)
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Deref (Derefed(..))
import Lamdu.Data.Infer.Load (LoadedDef)
import Utils
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
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

-- Run this function only once per M
inferDef ::
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a)) ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue, a))
inferDef act = do
  recursiveDefRef <- InferLoad.newDefinition recursiveDefI
  expr <- act
  _ <-
    expr ^. Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvType
    & Infer.tempUnify recursiveDefRef
    & mapStateT (Lens._Left %~ InferError)
  return expr

unifyExprVals ::
  Expr.Expression def (Infer.ScopedTypedValue, a) ->
  Expr.Expression def (Infer.ScopedTypedValue, a) ->
  M ()
unifyExprVals e1 e2 =
  Infer.tempUnify (e1 ^. exprValRef) (e2 ^. exprValRef)
  & mapStateT (Lens._Left %~ InferError)
  where
    exprValRef = Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvVal

try :: M a -> M (Either Error a)
try act = do
  oldState <- State.get
  let
    f (Left err) = Right (Left err, oldState)
    f (Right (res, newState)) = Right (Right res, newState)
  mapStateT f act

runNewContext :: M a -> Either Error a
runNewContext = (`evalStateT` Infer.emptyContext (Random.mkStdGen 0x1337))

-- Weaker and more convenient wrapper around runNewContext, deref,
-- inferDef, infer, load
loadInferDef :: Expr -> M (Expr.Expression Def (Derefed Def))
loadInferDef expr =
  (fmap . fmap) fst . deref =<< inferDef (infer =<< load expr)

runLoadInferDef :: Expr -> Either Error (Expr.Expression Def (Derefed Def))
runLoadInferDef = runNewContext . loadInferDef
