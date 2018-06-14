{-# LANGUAGE FlexibleContexts, TemplateHaskell, RankNTypes #-}
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
    Sugar.Expression name i o a ->
    Sugar.Expression name i o (Bool, a)
markStoredHoles expr =
    expr
    <&> (,) False
    & SugarLens.unfinishedExprPayloads . Sugar.plData . _1 .~ True

data NearestHoles = NearestHoles
    { _prev :: Maybe Sugar.EntityId
    , _next :: Maybe Sugar.EntityId
    } deriving (Eq, Show, Generic)
Lens.makeLenses ''NearestHoles

none :: NearestHoles
none = NearestHoles Nothing Nothing

add ::
    (forall a b.
      Lens.Traversal
      (f a)
      (f b)
      (Sugar.Expression name i o a)
      (Sugar.Expression name i o b)) ->
    f c ->
    f (c, NearestHoles)
add exprs s =
    s
    & exprs . Lens.mapped %~ toNearestHoles
    & exprs %~ markStoredHoles
    & passAll (exprs . SugarLens.subExprPayloads)
    & passAll (Lens.backwards (exprs . SugarLens.subExprPayloads))
    & exprs . Lens.mapped %~ snd
    where
        toNearestHoles x prevHole nextHole = (x, NearestHoles prevHole nextHole)

type M = State (Maybe Sugar.EntityId)

passAll ::
    LensLike M s t
    (Sugar.Payload name i o (Bool, Maybe Sugar.EntityId -> a))
    (Sugar.Payload name i o (Bool, a)) -> s -> t
passAll sugarPls s =
    s
    & sugarPls %%~ setEntityId
    & (`evalState` Nothing)

setEntityId ::
    Sugar.Payload name i o (Bool, Maybe Sugar.EntityId -> a) ->
    M (Sugar.Payload name i o (Bool, a))
setEntityId pl =
    do
        oldEntityId <- State.get
        when isStoredHole $ State.put $ Just $ pl ^. Sugar.plEntityId
        pl
            & Sugar.plData . _2 %~ ($ oldEntityId)
            & pure
    where
        isStoredHole = pl ^. Sugar.plData . _1
