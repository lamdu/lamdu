{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ExpressionGui.AddNextHoles
  ( addToDef, addToExpr
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Applicative.Utils (when)
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad ((<=<))
import Control.Monad.Trans.State (State, evalState)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.Sugar.Expression (subExpressions)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

addToDef ::
  MonadA m =>
  Sugar.Definition name m (Sugar.Expression name m ExprGuiM.Payload) ->
  Sugar.Definition name m (Sugar.Expression name m ExprGuiM.Payload)
addToDef =
  (`evalState` Nothing) .
  (Sugar.drBody . Lens.traversed) addToExprH

addToExpr ::
  MonadA m => Sugar.Expression name m ExprGuiM.Payload -> Sugar.Expression name m ExprGuiM.Payload
addToExpr = (`evalState` Nothing) . addToExprH

addToExprH ::
  Sugar.Expression name m ExprGuiM.Payload -> State (Maybe Guid) (Sugar.Expression name m ExprGuiM.Payload)
addToExprH =
  (Lens.backwards subExpressions %%@~ setGuid ExprGuiM.hgMNextHole) <=<
  (subExpressions %%@~ setGuid ExprGuiM.hgMPrevHole)

setGuid ::
  Lens' ExprGuiM.HoleGuids (Maybe Guid) ->
  Sugar.ExpressionP name m () ->
  Sugar.Payload name m ExprGuiM.Payload ->
  State (Maybe Guid) (Sugar.Payload name m ExprGuiM.Payload)
setGuid lens expr pl =
  setIt <$>
  State.get <*
  when (Lens.has Lens._Just (pl ^. Sugar.plActions) && isHoleToJumpTo expr)
    (State.put (Just (pl ^. Sugar.plGuid)))
  where
    setIt x = pl & Sugar.plData . ExprGuiM.plHoleGuids . lens .~ x

isHoleToJumpTo :: Sugar.ExpressionP name m a -> Bool
isHoleToJumpTo expr =
  Lens.has (Sugar.rBody . Sugar._BodyHole) expr ||
  Lens.anyOf (Sugar.rBody . Sugar._BodyInferred . Sugar.iValue . subExpressions . Lens.asIndex)
    isHoleToJumpTo expr
