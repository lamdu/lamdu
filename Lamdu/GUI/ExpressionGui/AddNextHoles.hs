{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ExpressionGui.AddNextHoles
  ( addToDef, addToExpr
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Applicative.Utils (when)
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad.Trans.State (State, evalState)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

addToDef ::
  MonadA m =>
  Sugar.Definition name m (Sugar.Expression name m ExprGuiM.Payload) ->
  Sugar.Definition name m (Sugar.Expression name m ExprGuiM.Payload)
addToDef =
  (Lens.traversed . Lens.traversed . Sugar.plData %~ snd) .
  (`evalState` Nothing) . Lens.traversed addNextHoles .
  (`evalState` Nothing) . Lens.traversed addPrevHoles .
  (Lens.traversed %~ addJumpToExprMarkers)

addToExpr ::
  MonadA m => Sugar.Expression name m ExprGuiM.Payload -> Sugar.Expression name m ExprGuiM.Payload
addToExpr =
  (Lens.traversed . Sugar.plData %~ snd) .
  (`evalState` Nothing) . addNextHoles .
  (`evalState` Nothing) . addPrevHoles .
  addJumpToExprMarkers

addNextHoles ::
  Sugar.Expression name m (Bool, ExprGuiM.Payload) -> State (Maybe Guid) (Sugar.Expression name m (Bool, ExprGuiM.Payload))
addNextHoles = Lens.backwards Lens.traversed %%~ setGuid ExprGuiM.hgMNextHole

addPrevHoles ::
  Sugar.Expression name m (Bool, ExprGuiM.Payload) -> State (Maybe Guid) (Sugar.Expression name m (Bool, ExprGuiM.Payload))
addPrevHoles = Lens.traversed %%~ setGuid ExprGuiM.hgMPrevHole

addJumpToExprMarkers ::
  Sugar.Expression name m ExprGuiM.Payload -> Sugar.Expression name m (Bool, ExprGuiM.Payload)
addJumpToExprMarkers =
  (Lens.traversed %~ removeNonStoredMarks) .
  (jumpToExprPayloads . Lens._1 .~ True) .
  (Lens.traversed . Sugar.plData %~ (,) False)
  where
    removeNonStoredMarks pl =
      case pl ^. Sugar.plActions of
      Nothing -> pl & Sugar.plData . Lens._1 .~ False
      Just _ -> pl

jumpToExprPayloads ::
  Lens.Traversal' (Sugar.Expression name m a) a
jumpToExprPayloads f expr =
  case expr ^. Sugar.rBody of
  Sugar.BodyHole _ -> mark
  Sugar.BodyInferred inferred
    | Lens.has jumpToExprPayloads (inferred ^. Sugar.iValue)
    -> mark
  Sugar.BodyCollapsed _ -> pure expr
  Sugar.BodyLam lam
    | lam ^. Sugar.lKind == Sugar.KVal
    -> (Sugar.rBody . Sugar._BodyLam . Sugar.lResultType) (jumpToExprPayloads f) expr
  _ -> (Sugar.rBody . Lens.traversed) (jumpToExprPayloads f) expr
  where
    mark = (Sugar.rPayload . Sugar.plData) f expr

setGuid ::
  Lens' ExprGuiM.HoleGuids (Maybe Guid) ->
  Sugar.Payload name m (Bool, ExprGuiM.Payload) ->
  State (Maybe Guid) (Sugar.Payload name m (Bool, ExprGuiM.Payload))
setGuid lens pl =
  setIt <$>
  State.get <*
  when (Lens.anyOf Sugar.plData fst pl)
    (State.put (Just (pl ^. Sugar.plGuid)))
  where
    setIt x = pl & Sugar.plData . Lens._2 . ExprGuiM.plHoleGuids . lens .~ x
