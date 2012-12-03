{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Editor.CodeEdit.Sugar.Infer
  (
  -- Type-check an expression into an ordinary Inferred Expression,
  -- short-circuit on error:
    inferMaybe, inferMaybe_

  -- Type-check an expression into a StoredResult:
  , inferLoadedExpression
  , StoredResult(..), srSuccess, srContext, srInferContext, srExpr, srBaseExpr, srBaseInferContext
  , StoredPayload, splGuid, splIWC, splProp

  -- Convert pure, inferred, stored expressions to Result:
  , resultFromStored, resultFromPure, resultFromInferred
  , Payload(..), Result, resultGuid, resultMInferred, resultPlMStored, resultProp
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Lens (SimpleLens)
import Control.Monad (liftM, void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Data.Cache (Cache)
import Data.Hashable (hash)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Data.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer
import qualified Editor.Data.Infer.ImplicitVariables as ImplicitVariables
import qualified Editor.Data.Load as Load
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type DefI = DataIRef.DefinitionIRef
type T = Transaction
type CT m = StateT Cache (T m)

type StoredPayload m = (InferredWithConflicts DefI, DataIRef.ExpressionProperty (T m))

splIWC :: SimpleLens (InferredWithConflicts DefI, a) (InferredWithConflicts DefI)
splIWC = Lens._1

splProp :: SimpleLens (StoredPayload m) (DataIRef.ExpressionProperty (T m))
splProp = Lens._2

type MInferredMStoredPayload m =
  Maybe
  ( InferredWithConflicts DefI
  , Maybe (DataIRef.ExpressionProperty (T m))
  )

data Payload m = Payload
  { plGuid :: Guid
  , plMInferred :: MInferredMStoredPayload m
  }

type Result m = Data.Expression DefI (Payload m)

data StoredResult m = StoredResult
  { _srSuccess :: Bool
  , _srContext :: Infer.Loaded DefI Load.PropertyClosure

  , _srInferContext :: Infer.Context DefI
  , _srExpr :: Result m
  -- Prior to adding variables
  , _srBaseInferContext :: Infer.Context DefI
  , _srBaseExpr :: Data.Expression DefI (StoredPayload m)
  }
LensTH.makeLenses ''StoredResult

resultFrom ::
  RandomGen g => g -> (a -> MInferredMStoredPayload m) ->
  Data.Expression DefI a -> Result m
resultFrom gen f =
    Data.randomizeParamIds paramGen
  . Data.randomizeExpr exprGen
  . (flip Payload . f <$>)
  where
    paramGen : exprGen : _ = RandomUtils.splits gen

-- Not inferred, not stored
resultFromPure :: RandomGen g => g -> Data.Expression DefI () -> Result m
resultFromPure gen =
  resultFrom gen (const Nothing)

-- Inferred, but not stored:
resultFromInferred :: Data.Expression DefI (Infer.Inferred DefI) -> Result m
resultFromInferred expr =
  resultFrom gen f expr
  where
    gen = Random.mkStdGen . hash . show $ void expr
    f inferred =
      Just
        ( InferredWithConflicts
          { iwcInferred = inferred
          , iwcTypeConflicts = []
          , iwcValueConflicts = []
          }
        , Nothing
        )

-- Inferred and stored
resultFromStored :: Data.Expression DefI (StoredPayload m) -> Result m
resultFromStored expr =
  f <$> expr
  where
    f i = Payload
      { plGuid = splGuid i
      , plMInferred = Just $ Just <$> i
      }

splGuid :: StoredPayload m -> Guid
splGuid = DataIRef.epGuid . snd

resultGuid :: Result m -> Guid
resultGuid = plGuid . Lens.view Data.ePayload

resultMInferred :: Result m -> MInferredMStoredPayload m
resultMInferred = plMInferred . Lens.view Data.ePayload

resultPlMStored :: Result m -> Maybe (StoredPayload m)
resultPlMStored = Lens.sequenceOf Lens._2 <=< plMInferred . Lens.view Data.ePayload

resultProp :: Result m -> Maybe (DataIRef.ExpressionProperty (T m))
resultProp = snd <=< plMInferred . Lens.view Data.ePayload

loader :: Monad m => Infer.Loader DefI (T m)
loader =
  Infer.Loader
  (liftM void . DataIRef.readExpression . Lens.view Data.defType <=<
   Transaction.readIRef)

inferMaybe ::
  Monad m =>
  Data.Expression DefI a -> Infer.Context DefI -> Infer.InferNode DefI ->
  T m (Maybe (Data.Expression DefI (Infer.Inferred DefI, a)))
inferMaybe expr inferContext inferPoint = do
  loaded <- Infer.load loader Nothing expr
  return . fmap fst . (`runStateT` inferContext) $
    Infer.inferLoaded (Infer.InferActions (const Nothing))
    loaded inferPoint

inferMaybe_ ::
  Monad m =>
  Data.Expression DefI () -> Infer.Context DefI -> Infer.InferNode DefI ->
  T m (Maybe (Data.Expression DefI (Infer.Inferred DefI)))
inferMaybe_ expr inferContext inferPoint =
  (liftM . fmap . fmap) fst $ inferMaybe expr inferContext inferPoint

inferWithVariables ::
  Monad m =>
  Infer.Loaded DefI a -> Infer.Context DefI -> Infer.InferNode DefI ->
  T m
  ( ( Bool
    , Infer.Context DefI
    , Data.Expression DefI (InferredWithConflicts DefI, a)
    )
  , ( Infer.Context DefI
    , Data.Expression DefI (InferredWithConflicts DefI, ImplicitVariables.Payload a)
    )
  )
inferWithVariables loaded baseRefMap node = do
  (wvContext, wvExpr) <-
    ImplicitVariables.addVariables loader newRefMap $
    (iwcInferred . fst &&& id) <$> expr
  return (res, (wvContext, asIWC <$> wvExpr))
  where
    res@(_, newRefMap, expr) = inferWithConflicts loaded baseRefMap node
    asIWC (newInferred, ImplicitVariables.Stored (oldIWC, a)) =
      ( oldIWC { iwcInferred = newInferred }
      , ImplicitVariables.Stored a
      )
    asIWC (newInferred, ImplicitVariables.AutoGen guid) =
      ( InferredWithConflicts newInferred [] []
      , ImplicitVariables.AutoGen guid
      )

inferLoadedExpression ::
  Monad m =>
  Maybe DefI -> Load.LoadedClosure ->
  (Infer.Context DefI, Infer.InferNode DefI) ->
  CT m (StoredResult m)
inferLoadedExpression mDefI lExpr inferState = do
  loaded <- lift $ Infer.load loader mDefI lExpr
  ((success, inferContext, expr), (wvInferContext, wvExpr)) <-
    Cache.memoS uncurriedInfer (loaded, inferState)
  return StoredResult
    { _srSuccess = success
    , _srContext = loaded

    , _srBaseInferContext = inferContext
    , _srBaseExpr = toProp (Lens.mapped . Lens._2) expr

    , _srInferContext = wvInferContext
    , _srExpr =
      toResult <$> toProp (Lens.mapped . Lens._2 . Lens.mapped) wvExpr
    }
  where
    toResult (iwc, ImplicitVariables.AutoGen guid) =
      Payload guid $ Just (iwc, Nothing)
    toResult (iwc, ImplicitVariables.Stored prop) =
      Payload (DataIRef.epGuid prop) $ Just (iwc, Just prop)
    toProp lens = Lens.over lens Load.propertyOfClosure
    uncurriedInfer (loaded, (inferContext, inferNode)) =
      inferWithVariables loaded inferContext inferNode
