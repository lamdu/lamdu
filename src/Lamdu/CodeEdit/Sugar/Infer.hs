{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Lamdu.CodeEdit.Sugar.Infer
  ( Payload(..), ntraversePayload, plGuid, plInferred, plStored, plSetter
  , ExpressionSetter
  , NoInferred(..), InferredWC
  , NoStored(..), Stored

  , inferLoadedExpression
  , InferLoadedResult(..)
  , ilrSuccess, ilrContext, ilrInferContext
  , ilrExpr, ilrBaseExpr, ilrBaseInferContext

  , resultFromPure, resultFromInferred

  -- TODO: These don't belong here:
  -- Type-check an expression into an ordinary Inferred Expression,
  -- short-circuit on error:
  , inferMaybe, inferMaybe_
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((&&&))
import Control.Monad (void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Hashable (hash)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Expression.Load as Load
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type T = Transaction
type CT m = StateT Cache (T m)

data NoInferred = NoInferred
type InferredWC t = InferredWithConflicts (DefI t)

data NoStored = NoStored
type Stored m = DataIRef.ExpressionProperty m

type ExpressionSetter def = Expression.Expression def () -> Expression.Expression def ()

data Payload t inferred stored
  = Payload
    { _plGuid :: Guid
    , _plInferred :: inferred
    , _plStored :: stored
    , _plSetter :: ExpressionSetter (DefI t)
    }
LensTH.makeLenses ''Payload

ntraversePayload ::
  Applicative f =>
  (inferreda -> f inferredb) ->
  (storeda -> f storedb) ->
  (ExpressionSetter (DefI ta) -> f (ExpressionSetter (DefI tb))) ->
  Payload ta inferreda storeda ->
  f (Payload tb inferredb storedb)
ntraversePayload onInferred onStored onSetter (Payload guid inferred stored setter) =
  Payload guid <$> onInferred inferred <*> onStored stored <*> onSetter setter

addPureExpressionSetters ::
  Expression.Expression def a ->
  Expression.Expression def (Lens.Context a (Expression.Expression def ()) (Expression.Expression def ()))
addPureExpressionSetters = Expression.addSubexpressionContexts (const ()) . Lens.Context id

randomizeGuids ::
  RandomGen g => g -> (a -> inferred) ->
  DataIRef.Expression t a ->
  DataIRef.Expression t (Payload t inferred NoStored)
randomizeGuids gen f =
    Expression.randomizeParamIds paramGen
  . Expression.randomizeExpr exprGen
  . fmap toPayload
  . addPureExpressionSetters
  . fmap f
  where
    toPayload (Lens.Context setter inferred) guid = Payload guid inferred NoStored setter
    paramGen : exprGen : _ = RandomUtils.splits gen

-- Not inferred, not stored
resultFromPure ::
  RandomGen g => g -> DataIRef.Expression t () ->
  DataIRef.Expression t (Payload t NoInferred NoStored)
resultFromPure = (`randomizeGuids` const NoInferred)

resultFromInferred ::
  DataIRef.Expression t (Infer.Inferred (DefI t)) ->
  DataIRef.Expression t (Payload t (InferredWC t) NoStored)
resultFromInferred expr =
  randomizeGuids gen f expr
  where
    gen = Random.mkStdGen . hash . show $ void expr
    f inferred =
      InferredWithConflicts
      { iwcInferred = inferred
      , iwcTypeConflicts = []
      , iwcValueConflicts = []
      }

-- {{{{{{{{{{{{{{{{{
-- TODO: These don't belong here
loader :: MonadA m => Infer.Loader (DefI (Tag m)) (T m)
loader =
  Infer.Loader
  (fmap void . DataIRef.readExpression . Lens.view Definition.defType <=<
   Transaction.readIRef)

inferMaybe ::
  MonadA m =>
  Maybe (DefI (Tag m)) ->
  DataIRef.ExpressionM m a ->
  Infer.Context (DefI (Tag m)) ->
  Infer.InferNode (DefI (Tag m)) ->
  T m (Maybe (DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), a)))
inferMaybe mDefI expr inferContext inferPoint = do
  loaded <- Infer.load loader mDefI expr
  return . fmap fst . (`runStateT` inferContext) $
    Infer.inferLoaded (Infer.InferActions (const Nothing))
    loaded inferPoint

inferMaybe_ ::
  MonadA m =>
  Maybe (DefI (Tag m)) ->
  DataIRef.ExpressionM m () ->
  Infer.Context (DefI (Tag m)) -> Infer.InferNode (DefI (Tag m)) ->
  T m (Maybe (DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))))
inferMaybe_ mDefI expr inferContext inferPoint =
  (fmap . fmap . fmap) fst $ inferMaybe mDefI expr inferContext inferPoint
-- }}}}}}}}}}}}}}}}}

inferWithVariables ::
  (RandomGen g, MonadA m) => g ->
  Infer.Loaded (DefI (Tag m)) a -> Infer.Context (DefI (Tag m)) -> Infer.InferNode (DefI (Tag m)) ->
  T m
  ( ( Bool
    , Infer.Context (DefI (Tag m))
    , DataIRef.ExpressionM m (InferredWithConflicts (DefI (Tag m)), a)
    )
  , ( Infer.Context (DefI (Tag m))
    , DataIRef.ExpressionM m (InferredWithConflicts (DefI (Tag m)), ImplicitVariables.Payload a)
    )
  )
inferWithVariables gen loaded baseInferContext node =
  (`evalStateT` baseInferContext) $ do
    (success, expr) <- toStateT $ inferWithConflicts loaded node
    intermediateContext <- State.get
    wvExpr <-
      ImplicitVariables.addVariables gen loader $
      (iwcInferred . fst &&& id) <$> expr
    wvContext <- State.get
    return
      ( (success, intermediateContext, expr)
      , (wvContext, asIWC <$> wvExpr)
      )
  where
    asIWC (newInferred, ImplicitVariables.Stored (oldIWC, a)) =
      ( oldIWC { iwcInferred = newInferred }
      , ImplicitVariables.Stored a
      )
    asIWC (newInferred, ImplicitVariables.AutoGen guid) =
      ( InferredWithConflicts newInferred [] []
      , ImplicitVariables.AutoGen guid
      )

data InferLoadedResult m = InferLoadedResult
  { _ilrSuccess :: Bool
  , _ilrContext :: Infer.Loaded (DefI (Tag m)) (Load.PropertyClosure (Tag m))
  , _ilrInferContext :: Infer.Context (DefI (Tag m))
  , _ilrExpr :: DataIRef.ExpressionM m (Payload (Tag m) (InferredWC (Tag m)) (Maybe (Stored m)))
  -- Prior to adding variables
  , _ilrBaseInferContext :: Infer.Context (DefI (Tag m))
  , _ilrBaseExpr :: DataIRef.ExpressionM m (Payload (Tag m) (InferredWC (Tag m)) (Stored m))
  }
LensTH.makeLenses ''InferLoadedResult

inferLoadedExpression ::
  (RandomGen g, MonadA m, Typeable (m ())) => g ->
  Maybe (DefI (Tag m)) -> Load.LoadedClosure (Tag m) ->
  (Infer.Context (DefI (Tag m)), Infer.InferNode (DefI (Tag m))) ->
  CT m (InferLoadedResult m)
inferLoadedExpression gen mDefI lExpr inferState = do
  loaded <- lift $ Infer.load loader mDefI lExpr
  ((success, inferContext, expr), (wvInferContext, wvExpr)) <-
    Cache.memoS uncurriedInfer (loaded, inferState)
  return InferLoadedResult
    { _ilrSuccess = success
    , _ilrContext = loaded

    , _ilrBaseInferContext = inferContext
    , _ilrBaseExpr = mkStoredPayload <$> addPureExpressionSetters expr

    , _ilrInferContext = wvInferContext
    , _ilrExpr = mkWVPayload <$> addPureExpressionSetters wvExpr
    }
  where
    uncurriedInfer (loaded, (inferContext, inferNode)) =
      inferWithVariables gen loaded inferContext inferNode

    mkStoredPayload (Lens.Context setter (iwc, propClosure)) =
      Payload (DataIRef.epGuid prop) iwc prop setter
      where
        prop = Load.propertyOfClosure propClosure
    mkWVPayload (Lens.Context setter (iwc, ImplicitVariables.AutoGen guid)) =
      Payload guid iwc Nothing setter
    mkWVPayload (Lens.Context setter (iwc, ImplicitVariables.Stored propClosure)) =
      Lens.over plStored Just $
      mkStoredPayload (Lens.Context setter (iwc, propClosure))
