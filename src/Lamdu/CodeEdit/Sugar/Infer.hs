{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Lamdu.CodeEdit.Sugar.Infer
  ( Payload(..), ntraversePayload, plGuid, plInferred, plStored
  , ExpressionSetter
  , NoInferred(..), InferredWC
  , NoStored(..), Stored
  , PayloadM, PayloadMM, ExprMM

  , isPolymorphicFunc
  , inferLoadedExpression
  , InferLoadedResult(..)
  , ilrSuccess, ilrContext, ilrInferContext
  , ilrExpr, ilrBaseExpr, ilrBaseInferContext

  , resultFromPure, resultFromInferred

  -- TODO: These don't belong here:
  -- Type-check an expression into an ordinary Inferred Expression,
  -- short-circuit on error:
  , load, inferMaybe, inferMaybe_

  , toPayloadMM
  , resultInferred
  , resultStored
  , resultGuid
  , exprStoredGuid

  , plIRef
  , resultMIRef
  , replaceWith
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Monad (void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Maybe (isJust)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag, Tagged)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified System.Random.Utils as RandomUtils

type T = Transaction
type CT m = StateT Cache (T m)

type PayloadM m = Payload (Tag m)
type PayloadMM m =
  PayloadM m (Maybe (InferredWC (Tag m))) (Maybe (Stored m))
type ExprMM m = DataIRef.ExpressionM m (PayloadMM m)

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
    }
LensTH.makeLenses ''Payload

ntraversePayload ::
  Applicative f =>
  (inferreda -> f inferredb) ->
  (storeda -> f storedb) ->
  Payload ta inferreda storeda ->
  f (Payload tb inferredb storedb)
ntraversePayload onInferred onStored (Payload guid inferred stored) =
  Payload guid <$> onInferred inferred <*> onStored stored

randomizeGuids ::
  RandomGen g => g -> (a -> inferred) ->
  DataIRef.Expression t a ->
  DataIRef.Expression t (Payload t inferred NoStored)
randomizeGuids gen f =
    ExprUtil.randomizeParamIds paramGen
  . ExprUtil.randomizeExpr exprGen
  . fmap (toPayload . f)
  where
    toPayload inferred guid = Payload guid inferred NoStored
    paramGen : exprGen : _ = RandomUtils.splits gen

-- Not inferred, not stored
resultFromPure ::
  RandomGen g => g -> DataIRef.Expression t () ->
  DataIRef.Expression t (Payload t NoInferred NoStored)
resultFromPure = (`randomizeGuids` const NoInferred)

resultFromInferred ::
  RandomGen g => g ->
  DataIRef.Expression t (Infer.Inferred (DefI t)) ->
  DataIRef.Expression t (Payload t (InferredWC t) NoStored)
resultFromInferred =
  flip randomizeGuids $ \inferred ->
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

load ::
  MonadA m => Maybe (DefI (Tag m)) ->
  DataIRef.ExpressionM m a ->
  T m (Infer.Loaded (DefI (Tag m)) a)
load = Infer.load loader

-- TODO: All uses of inferMaybe wrap it with memoization, so put
-- memoization here...
inferMaybe ::
  MonadA m =>
  Infer.Loaded (DefI (Tag m)) a ->
  Infer.Context (DefI (Tag m)) ->
  Infer.InferNode (DefI (Tag m)) ->
  Maybe (DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), a))
inferMaybe loaded inferContext inferPoint =
  fmap fst . (`runStateT` inferContext) $
  Infer.inferLoaded (Infer.InferActions (const Nothing))
  loaded inferPoint

inferMaybe_ ::
  MonadA m =>
  Infer.Loaded (DefI (Tag m)) a ->
  Infer.Context (DefI (Tag m)) ->
  Infer.InferNode (DefI (Tag m)) ->
  Maybe (DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))))
inferMaybe_ loaded inferContext inferPoint =
  (fmap . fmap) fst $ inferMaybe loaded inferContext inferPoint
-- }}}}}}}}}}}}}}}}}

inferWithVariables ::
  (RandomGen g, MonadA m) => g ->
  Infer.Loaded (DefI (Tag m)) a -> Infer.Context (DefI (Tag m)) -> Infer.InferNode (DefI (Tag m)) ->
  T m
  ( ( Infer.Context (DefI (Tag m))
    , DataIRef.ExpressionM m (InferredWithConflicts (DefI (Tag m)), a)
    )
  , Maybe
    ( Infer.Context (DefI (Tag m))
    , DataIRef.ExpressionM m (InferredWithConflicts (DefI (Tag m)), ImplicitVariables.Payload a)
    )
  )
inferWithVariables gen loaded baseInferContext node =
  (`evalStateT` baseInferContext) $ do
    (success, expr) <- toStateT $ inferWithConflicts loaded node
    intermediateContext <- State.get
    mWithVariables <- if success
      then do
        wvExpr <-
          ImplicitVariables.addVariables gen loader $
          (iwcInferred . fst &&& id) <$> expr
        wvContext <- State.get
        return $ Just (wvContext, asIWC <$> wvExpr)
      else
        return Nothing
    return
      ( (intermediateContext, expr)
      , mWithVariables
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
  loaded <- lift $ load mDefI lExpr
  ((baseContext, expr), mWithVariables) <-
    Cache.memoS uncurriedInfer (loaded, inferState)
  let baseExpr = mkStoredPayload <$> expr
  return InferLoadedResult
    { _ilrSuccess = isJust mWithVariables
    , _ilrContext = loaded

    , _ilrBaseInferContext = baseContext
    , _ilrBaseExpr = baseExpr

    , _ilrInferContext = maybe baseContext fst mWithVariables
    , _ilrExpr =
      maybe (baseExpr & Lens.mapped . plStored %~ Just)
      (fmap mkWVPayload . snd) mWithVariables
    }
  where
    uncurriedInfer (loaded, (inferContext, inferNode)) =
      inferWithVariables gen loaded inferContext inferNode

    mkStoredPayload (iwc, propClosure) =
      Payload (DataIRef.epGuid prop) iwc prop
      where
        prop = Load.propertyOfClosure propClosure
    mkWVPayload (iwc, ImplicitVariables.AutoGen guid) =
      Payload guid iwc Nothing
    mkWVPayload (iwc, ImplicitVariables.Stored propClosure) =
      Lens.over plStored Just $
      mkStoredPayload (iwc, propClosure)

isPolymorphicFunc :: ExprMM m -> Bool
isPolymorphicFunc funcI =
  maybe False
  (ExprUtil.isDependentPi . Infer.iType . iwcInferred) $
  resultInferred funcI

resultGuid ::
  Expression.Expression def (Payload t inferred stored) -> Guid
resultGuid = (^. Expression.ePayload . plGuid)

resultStored ::
  Expression.Expression def (Payload t inferred stored) -> stored
resultStored = (^. Expression.ePayload . plStored)

resultInferred ::
  Expression.Expression def (Payload t inferred stored) -> inferred
resultInferred = (^. Expression.ePayload . plInferred)

toPayloadMM :: Payload (Tagged (m ())) NoInferred NoStored -> PayloadMM m
toPayloadMM =
  Lens.set plInferred Nothing .
  Lens.set plStored Nothing

plIRef ::
  Lens.Traversal'
  (Expression.Expression def (Payload t i (Maybe (Stored m))))
  (DataIRef.ExpressionI (Tag m))
plIRef = Expression.ePayload . plStored . traverse . Property.pVal

exprStoredGuid ::
  Lens.Fold
  (Expression.Expression def (Payload t i (Maybe (Stored m)))) Guid
exprStoredGuid = plIRef . Lens.to DataIRef.exprGuid

replaceWith :: MonadA m => Stored m -> Stored m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

resultMIRef ::
  ExprMM m ->
  Maybe (DataIRef.ExpressionIM m)
resultMIRef = fmap Property.value . resultStored
