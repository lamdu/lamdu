{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, TemplateHaskell, RankNTypes #-}
module Lamdu.Sugar.NearestHoles
    ( NearestHoles(..), prev, next
    , none
    , add
    ) where

import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.State (State, evalState)
import qualified Control.Monad.Trans.State as State
import           Control.MonadA (MonadA)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

data NearestHoles = NearestHoles
    { _prev :: Maybe Sugar.EntityId
    , _next :: Maybe Sugar.EntityId
    } deriving (Eq, Show)
Lens.makeLenses ''NearestHoles

none :: NearestHoles
none = NearestHoles Nothing Nothing

add ::
    MonadA m =>
    (forall a b.
      Lens.Traversal
      (f (Sugar.Expression name m a))
      (f (Sugar.Expression name m b))
      (Sugar.Expression name m a)
      (Sugar.Expression name m b)) ->
    f (Sugar.Expression name m (NearestHoles -> r)) ->
    f (Sugar.Expression name m r)
add exprs s =
    s
    & exprs . Lens.mapped %~ toNearestHoles
    & passAll (exprs . SugarLens.subExprPayloads)
    & passAll (Lens.backwards (exprs . SugarLens.subExprPayloads))
    where
        toNearestHoles f prevHole nextHole = f (NearestHoles prevHole nextHole)

type M = State (Maybe Sugar.EntityId)

passAll ::
    LensLike M s t
    (Sugar.Payload m (Maybe Sugar.EntityId -> a))
    (Sugar.Payload m a) -> s -> t
passAll sugarPls s =
    s
    & sugarPls %%~ setEntityId
    & (`evalState` Nothing)

setEntityId ::
    Sugar.Payload m (Maybe Sugar.EntityId -> a) ->
    M (Sugar.Payload m a)
setEntityId pl =
    do
        oldEntityId <- State.get
        State.put $ Just $ pl ^. Sugar.plEntityId
        pl
            & Sugar.plData %~ ($ oldEntityId)
            & return
