module InferWrappers where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT, mapStateT, evalStateT)
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Deref (DerefedTV(..))
import Lamdu.Data.Infer.Load (LoadedExpr, ldDef)
import System.Random (RandomGen)
import Utils
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Deref as InferDeref
import qualified Lamdu.Data.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Infer.Structure as Structure
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

fromRight :: Show err => Either err a -> a
fromRight = either (error . show) id

data Error
  = LoadError (InferLoad.Error Def)
  | InferError (Infer.Error Def)
  deriving (Show)

type M = StateT (Infer.Context Def) (Either Error)

load :: Expr.Expression Def a -> M (LoadedExpr Def a)
load expr =
  InferLoad.load loader expr
  & mapStateT ((Lens._Left %~ LoadError) . fromRight . runEitherT)

fromRightM :: (Monad m, Show l) => StateT s (Either l) a -> StateT s m a
fromRightM = mapStateT (return . fromRight)

fromRightT :: (MonadA m, Show l) => StateT s (EitherT l m) a -> StateT s m a
fromRightT = mapStateT (fmap fromRight . runEitherT)

type InferredLoadedExpr a = LoadedExpr Def (Infer.TypedValue Def, a)

inferScopedBy :: Infer.ExprRef Def -> LoadedExpr Def a -> M (InferredLoadedExpr a)
inferScopedBy valRef expr = do
  scope <- Infer.getScope valRef
  inferScope scope expr

inferScope :: Infer.Scope Def -> LoadedExpr Def a -> M (InferredLoadedExpr a)
inferScope scope expr =
  Infer.infer scope expr & mapStateT (Lens._Left %~ InferError)

infer :: LoadedExpr Def a -> M (InferredLoadedExpr a)
infer = inferScope $ Infer.emptyScope recursiveDefI

mapDerefError :: InferDeref.M Def a -> M a
mapDerefError = mapStateT (Lens._Left %~ InferError . InferDeref.toInferError)

derefWithPL :: InferredLoadedExpr a -> M (Expr.Expression Def (DerefedTV Def, a))
derefWithPL expr = expr
  & ExprLens.exprDef %~ (^. InferLoad.ldDef)
  & InferDeref.entireExpr
  & mapDerefError

deref ::
  LoadedExpr Def (Infer.TypedValue Def) ->
  M ExprInferred
deref expr =
  expr
  <&> flip (,) ()
  & derefWithPL
  <&> fmap
  (\(DerefedTV val typ _scope _tv _ctx, ()) ->
    ( val & ExprLens.exprDef %~ (^. ldDef) & void
    , typ & ExprLens.exprDef %~ (^. ldDef) & void
    )
  )

-- Run this function only once per M
inferDef :: M (InferredLoadedExpr a) -> M (InferredLoadedExpr a)
inferDef act = do
  recursiveDefTV <- InferLoad.newDefinition recursiveDefI
  expr <- act
  _ <-
    expr ^. Expr.ePayload . Lens._1
    & unify recursiveDefTV
  return expr

unify :: Infer.TypedValue Def -> Infer.TypedValue Def -> M ()
unify e1 e2 = Infer.unify e1 e2 & mapStateT (Lens._Left %~ InferError) & void

addImplicitVariables ::
  RandomGen gen =>
  gen -> Def -> InferredLoadedExpr c ->
  M (InferredLoadedExpr (ImplicitVariables.Payload c))
addImplicitVariables =
  ImplicitVariables.add
  & Lens.mapped . Lens.mapped . Lens.mapped %~
    mapStateT (Lens._Left %~ InferError)

addStructure ::
  LoadedExpr Def (Infer.TypedValue Def, a) ->
  M (LoadedExpr Def (Infer.TypedValue Def, a))
addStructure expr =
  expr <$ mapDerefError (Structure.add expr)

loadInferInContext ::
  Infer.TypedValue Def -> Expr.Expression Def a -> M (InferredLoadedExpr a)
loadInferInContext tv expr = inferScopedBy (tv ^. Infer.tvVal) =<< load expr

loadInferInto ::
  Infer.TypedValue Def -> Expr.Expression Def a -> M (InferredLoadedExpr a)
loadInferInto stv expr = do
  resumptionInferred <- loadInferInContext stv expr
  unify (resumptionInferred ^. Expr.ePayload . Lens._1) stv
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

loadInferDef :: Expr a -> M (InferredLoadedExpr a)
loadInferDef expr = inferDef (infer =<< load expr)

-- Weaker and more convenient wrapper around runNewContext, deref,
-- inferDef, infer, load
loadInferDerefDef :: Expr () -> M ExprInferred
loadInferDerefDef expr = deref . fmap fst =<< loadInferDef expr

runLoadInferDerefDef :: Expr () -> Either Error ExprInferred
runLoadInferDerefDef = runNewContext . loadInferDerefDef
