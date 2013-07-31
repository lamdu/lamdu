module InferWrappers where

import Control.Lens.Operators
import Control.Monad (void)
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

type ExprInferred = Expr (Expr (), Expr ())

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

data Error
  = LoadError (InferLoad.Error Def)
  | InferError (Infer.Error Def)
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

inferScope ::
  Infer.Scope Def -> Expr.Expression (LoadedDef Def) a ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a))
inferScope scope expr =
  Infer.infer scope expr & mapStateT (Lens._Left %~ InferError)

infer ::
  Expr.Expression (LoadedDef Def) a ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a))
infer = inferScope Infer.emptyScope

derefWithPL ::
  Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a) ->
  M (Expr.Expression Def (Derefed Def, a))
derefWithPL expr = expr
  & ExprLens.exprDef %~ (^. InferLoad.ldDef)
  & InferDeref.expr
  >>= Lens.sequenceOf (Lens.traverse . Lens._1)
  & mapStateT (Lens._Left %~ mapErr)
  where
    mapErr (InferDeref.InfiniteExpression ref) = InferError (Infer.InfiniteExpression ref)

deref ::
  Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def) ->
  M ExprInferred
deref expr =
  expr
  <&> flip (,) ()
  & derefWithPL
  <&> fmap (\(Derefed val typ, ()) -> (void val, void typ))

-- Run this function only once per M
inferDef ::
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a)) ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a))
inferDef act = do
  recursiveDefRef <- InferLoad.newDefinition recursiveDefI
  expr <- act
  _ <-
    expr ^. Expr.ePayload . Lens._1 . Infer.stvTV
    & unify recursiveDefRef
  return expr

unify :: Infer.TypedValue Def -> Infer.TypedValue Def -> M ()
unify e1 e2 = Infer.unify e1 e2 & mapStateT (Lens._Left %~ InferError)

loadInferInContext ::
  Infer.ScopedTypedValue Def -> Expr.Expression Def a ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a))
loadInferInContext stv expr = inferScope (stv ^. Infer.stvScope) =<< load expr

loadInferInto ::
  Infer.ScopedTypedValue Def -> Expr.Expression Def a ->
  M (Expr.Expression (LoadedDef Def) (Infer.ScopedTypedValue Def, a))
loadInferInto stv expr = do
  resumptionInferred <- loadInferInContext stv expr
  unify (resumptionInferred ^. Expr.ePayload . Lens._1 . Infer.stvTV) (stv ^. Infer.stvTV)
  return resumptionInferred

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
loadInferDerefDef :: Expr () -> M ExprInferred
loadInferDerefDef expr =
  deref . fmap fst =<< inferDef (infer =<< load expr)

runLoadInferDerefDef :: Expr () -> Either Error ExprInferred
runLoadInferDerefDef = runNewContext . loadInferDerefDef
