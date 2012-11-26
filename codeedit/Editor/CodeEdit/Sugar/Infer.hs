{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Editor.CodeEdit.Sugar.Infer
   ( ExprEntity
   , ExprEntityStored
   , InferExpressionResult(..), ierContext, ierExpr, ierRefmap, ierSuccess
   , EntityPayload(..)
   , inferMaybe
   , inferLoadedExpression
   , iwcGuid, eeGuid, eeStored, eeProp
   , eeFromPure
   ) where

import Control.Applicative ((<$>), (<$))
import Control.Arrow ((&&&))
import Control.Monad (liftM, void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)
import Data.Cache (Cache)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer
import qualified Editor.Data.Load as Load
import qualified System.Random.Utils as RandomUtils

type T = Transaction ViewTag
type CT m = StateT Cache (T m)

type ExprEntityStored m =
  InferredWithConflicts (DataIRef.ExpressionProperty (T m))

type ExprEntityMStored m =
  InferredWithConflicts (Maybe (DataIRef.ExpressionProperty (T m)))

data InferExpressionResult m = InferExpressionResult
  { _ierSuccess :: Bool
  , _ierRefmap :: Infer.RefMap
  , _ierExpr :: Data.Expression (ExprEntityStored m)
  , _ierContext :: Infer.Loaded Data.ExpressionIRef
  }
LensTH.makeLenses ''InferExpressionResult

data EntityPayload m = EntityPayload
  { eplGuid :: Guid
  , eplInferred :: Maybe (ExprEntityMStored m)
  }

type ExprEntity m = Data.Expression (EntityPayload m)

iwcGuid :: ExprEntityStored m -> Guid
iwcGuid = DataIRef.epGuid . Infer.iStored . iwcInferred

eeGuid :: ExprEntity m -> Guid
eeGuid = eplGuid . Lens.view Data.ePayload

eeStored :: ExprEntity m -> Maybe (ExprEntityStored m)
eeStored = Traversable.sequenceA <=< eplInferred . Lens.view Data.ePayload

eeProp :: ExprEntity m -> Maybe (DataIRef.ExpressionProperty (T m))
eeProp = Infer.iStored . iwcInferred <=< eplInferred . Lens.view Data.ePayload

eeFromPure :: RandomGen g => g -> Data.PureExpression -> ExprEntity m
eeFromPure gen =
    Data.randomizeParamIds paramGen
  . Data.randomizeExpr exprGen
  . ((`EntityPayload` Nothing) <$)
  where
    paramGen : exprGen : _ = RandomUtils.splits gen

inferredIRefToStored ::
  Monad m => Load.ExpressionSetter (T m) ->
  Data.Expression (InferredWithConflicts Data.ExpressionIRef) ->
  Data.Expression (ExprEntityStored m)
inferredIRefToStored setter expr =
  fmap propIntoInferred . Load.exprAddProp . Load.Stored setter $
  (Infer.iStored . iwcInferred &&& id) <$> expr
  where
    propIntoInferred (prop, eei) = prop <$ eei

loader :: Monad m => Infer.Loader (T m)
loader =
  Infer.Loader
  (liftM void . Load.loadExpressionIRef . Data.defType <=<
   Transaction.readIRef)

inferMaybe ::
  Monad m => Data.Expression a -> Infer.RefMap ->
  Infer.InferNode -> T m (Maybe (Infer.Expression a))
inferMaybe expr inferContext inferPoint = do
  loaded <- Infer.load loader Nothing expr
  return . fmap fst $
    Infer.infer (Infer.InferActions (const Nothing))
    loaded inferContext inferPoint

inferLoadedExpression ::
  Monad m =>
  Maybe Data.DefinitionIRef -> Load.Loaded (T m) ->
  (Infer.RefMap, Infer.InferNode) ->
  CT m (InferExpressionResult m)
inferLoadedExpression mDefI (Load.Stored setExpr exprIRef) inferState = do
  loaded <- lift $ Infer.load loader mDefI exprIRef
  (success, refMap, expr) <-
    Cache.memoS (return . uncurriedInfer) (loaded, inferState)
  return InferExpressionResult
    { _ierSuccess = success
    , _ierRefmap = refMap
    , _ierExpr = inferredIRefToStored setExpr expr
    , _ierContext = loaded
    }
  where
    uncurriedInfer (loaded, (refMap, inferNode)) =
      inferWithConflicts loaded refMap inferNode
