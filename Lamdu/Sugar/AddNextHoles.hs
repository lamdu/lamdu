module Lamdu.Sugar.AddNextHoles
  ( addToDef
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.Sugar.Convert.Expression (subExpressions)
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Traversable as Traversable

processExpr ::
  MonadA m =>
  Maybe Guid -> Expression name m a ->
  (Maybe Guid, Expression name m a)
processExpr mNextHole expr =
  ( newMNextHole
  , expr & rBody .~ newBody
  )
  where
    (newMNextHole, newBody) =
      Traversable.mapAccumR step mNextHole $ expr ^. rBody
    step prevMNextHole curExpr =
      curExpr
      & rPayload . plMNextHoleGuid .~ prevMNextHole
      & processExpr prevMNextHole
      & if isHoleToJumpTo curExpr
        then Lens._1 .~ Just (curExpr ^. rPayload . plGuid)
        else id

isHoleToJumpTo :: Expression name m a -> Bool
isHoleToJumpTo expr =
  Lens.has (rBody . _BodyHole) expr ||
  Lens.anyOf (rBody . _BodyInferred . iValue . Lens.folding subExpressions) isHoleToJumpTo expr

toExpr ::
  MonadA m =>
  Expression name m a -> Expression name m a
toExpr = snd . processExpr Nothing

toDefContent ::
  MonadA m =>
  DefinitionContent name m (Expression name m a) ->
  DefinitionContent name m (Expression name m a)
toDefContent =
  (dBody %~ toExpr) .
  (dWhereItems . Lens.traversed . wiValue %~ toDefContent)

addToDef ::
  MonadA m =>
  Definition name m (Expression name m a) ->
  Definition name m (Expression name m a)
addToDef =
  drBody . _DefinitionBodyExpression . deContent %~ toDefContent
