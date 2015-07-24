{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, TemplateHaskell, RankNTypes #-}
module Lamdu.Sugar.NearestHoles
    ( NearestHoles(..), prev, next
    , none
    , add
    ) where

import           Prelude.Compat

import           Control.Applicative.Utils (when)
import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.State (State, evalState)
import qualified Control.Monad.Trans.State as State
import           Control.MonadA (MonadA)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

markStoredHoles ::
    Sugar.Expression name m a ->
    Sugar.Expression name m (Bool, a)
markStoredHoles expr =
    expr
    <&> (,) False
    & SugarLens.holePayloads . Sugar.plData . _1 .~ True
    & SugarLens.subExprPayloads %~ removeNonStoredMarks
    where
        removeNonStoredMarks pl =
            case pl ^. Sugar.plActions of
            Nothing -> pl & Sugar.plData . _1 .~ False
            Just _ -> pl

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
    & exprs %~ markStoredHoles
    & passAll (exprs . SugarLens.subExprPayloads)
    & passAll (Lens.backwards (exprs . SugarLens.subExprPayloads))
    & exprs . Lens.mapped %~ snd
    where
        toNearestHoles f prevHole nextHole = f (NearestHoles prevHole nextHole)

type M = State (Maybe Sugar.EntityId)

passAll ::
    LensLike M s t
    (Sugar.Payload m (Bool, Maybe Sugar.EntityId -> a))
    (Sugar.Payload m (Bool, a)) -> s -> t
passAll sugarPls s =
    s
    & sugarPls %%~ setEntityId
    & (`evalState` Nothing)

setEntityId ::
    Sugar.Payload m (Bool, Maybe Sugar.EntityId -> a) ->
    M (Sugar.Payload m (Bool, a))
setEntityId pl =
    do
        oldEntityId <- State.get
        when isStoredHole $ State.put $ Just $ pl ^. Sugar.plEntityId
        pl
            & Sugar.plData . _2 %~ ($ oldEntityId)
            & return
    where
        isStoredHole = pl ^. Sugar.plData . _1
