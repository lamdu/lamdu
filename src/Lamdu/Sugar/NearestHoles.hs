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
    Lens.AnIndexedTraversal (SugarLens.PayloadOf name i o) s t a (Bool, a) ->
    s -> t
markStoredHoles pls =
    Lens.cloneIndexedTraversal pls %@~
    \i pl -> (Lens.has (SugarLens._OfExpr . SugarLens.bodyUnfinished) i, pl)

data NearestHoles = NearestHoles
    { _prev :: Maybe Sugar.EntityId
    , _next :: Maybe Sugar.EntityId
    } deriving (Eq, Show, Generic)
Lens.makeLenses ''NearestHoles

none :: NearestHoles
none = NearestHoles Nothing Nothing

add ::
    Functor f =>
    (forall a b.
      Lens.IndexedTraversal (SugarLens.PayloadOf name i o) (f a) (f b) a b) ->
    f (Sugar.Payload name i o c) ->
    f (Sugar.Payload name i o (c, NearestHoles))
add exprs s =
    s
    & markStoredHoles exprs
    & passAll exprs
    & passAll (Lens.backwards exprs)
    <&> snd
    <&> Sugar.plData %~ toNearestHoles
    where
        toNearestHoles (nextHole, (prevHole, x)) = (x, NearestHoles prevHole nextHole)

type M = State (Maybe Sugar.EntityId)

passAll ::
    LensLike M s t
    (Bool, Sugar.Payload name i o a)
    (Bool, Sugar.Payload name i o (Maybe Sugar.EntityId, a)) -> s -> t
passAll sugarPls s =
    s
    & sugarPls %%~ setEntityId
    & (`evalState` Nothing)

setEntityId ::
    (Bool, Sugar.Payload name i o a) ->
    M (Bool, Sugar.Payload name i o (Maybe Sugar.EntityId, a))
setEntityId (isStoredHole, pl) =
    do
        oldEntityId <- State.get
        when isStoredHole (State.put (Just (pl ^. Sugar.plEntityId)))
        pure (isStoredHole, pl & Sugar.plData %~ (,) oldEntityId)
