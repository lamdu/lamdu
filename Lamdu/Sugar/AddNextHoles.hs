{-# LANGUAGE FlexibleContexts #-}
module Lamdu.Sugar.AddNextHoles
  ( addToDef
  ) where

import Control.Applicative (Applicative(..), (<$>))
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
  ExpressionP name m () ->
  Payload name m a ->
  State (Maybe Guid) (Payload name m a)
setNextHole expr pl =
  setIt <$>
  State.get <*
  when (isJust (pl ^. plActions) && isHoleToJumpTo expr)
    (State.put (Just (pl ^. plGuid)))
  where
    setIt x = pl & plMNextHoleGuid .~ x

isHoleToJumpTo :: ExpressionP name m a -> Bool
isHoleToJumpTo expr =
  Lens.has (rBody . _BodyHole) expr ||
  Lens.anyOf (rBody . _BodyInferred . iValue . subExpressions . Lens.asIndex)
    isHoleToJumpTo expr
