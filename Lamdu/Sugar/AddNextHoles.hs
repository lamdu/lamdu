{-# LANGUAGE FlexibleContexts #-}
module Lamdu.Sugar.AddNextHoles
  ( addToDef
  ) where

import Control.Applicative (Applicative(..))
import Control.Applicative.Utils (when)
import Control.Lens.Operators
import Control.Monad.Trans.State (State, evalState)
import Control.MonadA (MonadA)
import Data.Maybe (isJust)
import Data.Store.Guid (Guid)
import Lamdu.Sugar.Convert.Expression (subExpressions)
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State

addToDef ::
  MonadA m =>
  Definition name m (Expression name m a) ->
  Definition name m (Expression name m a)
addToDef =
  (`evalState` Nothing) .
  (drBody . Lens.traversed . Lens.backwards subExpressions %%@~ setNextHole)

setNextHole ::
  Body name m (ExpressionP name m ()) ->
  Payload name m a ->
  State (Maybe Guid) (Payload name m a)
setNextHole body pl =
  State.get <*
  when (isJust (pl ^. plActions) && isHoleToJumpTo body)
    (State.put (Just (pl ^. plGuid)))
  <&> useMNextGuid
  where
    useMNextGuid Nothing = pl
    useMNextGuid nextGuid = pl & plMNextHoleGuid .~ nextGuid

isHoleToJumpTo :: Body name m (ExpressionP name m a) -> Bool
isHoleToJumpTo expr =
  Lens.has _BodyHole expr ||
  Lens.anyOf (_BodyInferred . iValue . subExpressions . Lens.asIndex) isHoleToJumpTo expr
