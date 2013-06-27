module Lamdu.Sugar.AddNextHoles
  ( addToDef
  ) where

import Control.Applicative ((<$), (<|>))
import Control.Lens.Operators
import Control.Monad (guard)
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
processExpr mNextHole (Expression body pl) =
  ( newMNextHole
  , Expression newBody pl
  )
  where
    selfHole = pl ^. plGuid <$ guard (isHoleToJumpTo body)
    (newMNextHole, newBody) =
      Traversable.mapAccumR step (selfHole <|> mNextHole) body
    step prevMNextHole curExpr =
      curExpr
      & rPayload . plMNextHoleGuid .~ prevMNextHole
      & processExpr prevMNextHole
      & if isHoleToJumpTo $ curExpr ^. rBody
        then Lens._1 .~ Just (curExpr ^. rPayload . plGuid)
        else id

isHoleToJumpTo :: Body namea m (ExpressionP nameb n b) -> Bool
isHoleToJumpTo expr =
  Lens.has (_BodyHole) expr ||
  Lens.anyOf (_BodyInferred . iValue . subExpressions . Lens.asIndex) isHoleToJumpTo expr

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
