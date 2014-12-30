{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ExpressionGui.AddNextHoles
  ( addToDef, addToExpr
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Applicative.Utils (when)
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Trans.State (State, evalState)
import Control.MonadA (MonadA)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

markStoredHoles ::
  Sugar.Expression name m a ->
  Sugar.Expression name m (Bool, a)
markStoredHoles expr =
  expr
  <&> Sugar.plData %~ (,) False
  & SugarLens.holePayloads . Sugar.plData . _1 .~ True
  <&> removeNonStoredMarks
  where
    removeNonStoredMarks pl =
      case pl ^. Sugar.plActions of
      Nothing -> pl & Sugar.plData . _1 .~ False
      Just _ -> pl

addToDef ::
  MonadA m =>
  Sugar.Definition name m (Sugar.Expression name m ExprGuiM.Payload) ->
  Sugar.Definition name m (Sugar.Expression name m ExprGuiM.Payload)
addToDef def =
  def
  <&> markStoredHoles
  & addPrevHoles (Lens.traverse . Lens.traverse)
  & addNextHoles (Lens.traverse . Lens.traverse)
  <&> Lens.mapped . Sugar.plData %~ snd

addToExpr ::
  MonadA m =>
  Sugar.Expression name m ExprGuiM.Payload ->
  Sugar.Expression name m ExprGuiM.Payload
addToExpr expr =
  expr
  & markStoredHoles
  & addPrevHoles Lens.traverse
  & addNextHoles Lens.traverse
  <&> Sugar.plData %~ snd

addNextHoles :: Lens.Traversal' s (Sugar.Payload m (Bool, ExprGuiM.Payload)) -> s -> s
addNextHoles traversal s =
  s
  & Lens.backwards traversal %%~ setEntityId ExprGuiM.hgMNextHole
  & (`evalState` Nothing)

addPrevHoles :: Lens.Traversal' s (Sugar.Payload m (Bool, ExprGuiM.Payload)) -> s -> s
addPrevHoles traversal s =
  s
  & traversal %%~ setEntityId ExprGuiM.hgMPrevHole
  & (`evalState` Nothing)

setEntityId ::
  Lens' ExprGuiM.HoleEntityIds (Maybe Sugar.EntityId) ->
  Sugar.Payload m (Bool, ExprGuiM.Payload) ->
  State (Maybe Sugar.EntityId) (Sugar.Payload m (Bool, ExprGuiM.Payload))
setEntityId lens pl =
  setIt <$>
  State.get <*
  when (Lens.anyOf Sugar.plData fst pl)
    (State.put (Just (pl ^. Sugar.plEntityId)))
  where
    setIt x = pl & Sugar.plData . _2 . ExprGuiM.plHoleEntityIds . lens .~ x
