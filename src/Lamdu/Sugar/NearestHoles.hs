{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, TemplateHaskell, RankNTypes #-}
module Lamdu.Sugar.NearestHoles
    ( NearestHoles(..), prev, next
    , none
    , add
    ) where

import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (State, evalState)
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

markStoredHoles ::
    Sugar.Expression name m a ->
    Sugar.Expression name m (Bool, a)
markStoredHoles expr =
    expr
    <&> (,) False
    & SugarLens.holeAndWrapperPayloads . Sugar.plData . _1 .~ True

data NearestHoles = NearestHoles
    { _prev :: Maybe Sugar.EntityId
    , _next :: Maybe Sugar.EntityId
    } deriving (Eq, Show)
Lens.makeLenses ''NearestHoles

none :: NearestHoles
none = NearestHoles Nothing Nothing

add ::
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
    & exprs %~ markStoredHoles
    & passAll (exprs . SugarLens.subExprPayloads)
    & passAll (Lens.backwards (exprs . SugarLens.subExprPayloads))
    & exprs . Lens.mapped %~ snd
    where
        toNearestHoles f prevHole nextHole = f (NearestHoles prevHole nextHole)

type M = State (Maybe Sugar.EntityId)

passAll ::
    LensLike M s t
    (Sugar.Payload f (Bool, Maybe Sugar.EntityId -> a))
    (Sugar.Payload f (Bool, a)) -> s -> t
passAll sugarPls s =
    s
    & sugarPls %%~ setEntityId
    & (`evalState` Nothing)

setEntityId ::
    Sugar.Payload f (Bool, Maybe Sugar.EntityId -> a) ->
    M (Sugar.Payload f (Bool, a))
setEntityId pl =
    do
        oldEntityId <- State.get
        when isStoredHole $ State.put $ Just $ pl ^. Sugar.plEntityId
        pl
            & Sugar.plData . _2 %~ ($ oldEntityId)
            & return
    where
        isStoredHole = pl ^. Sugar.plData . _1
