{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ExpressionGui.AddNextHoles
  ( add
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

add ::
  MonadA m =>
  (forall a b.
   Lens.Traversal (f a) (f b)
   (Sugar.Expression name m a)
   (Sugar.Expression name m b)) ->
  f ExprGuiM.Payload -> f ExprGuiM.Payload
add traversal s =
  s
  & traversal %~ markStoredHoles
  & addPrevHoles (traversal . Lens.traverse)
  & addNextHoles (traversal . Lens.traverse)
  & traversal . Lens.mapped . Sugar.plData %~ snd

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
