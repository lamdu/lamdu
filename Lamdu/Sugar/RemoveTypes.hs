module Lamdu.Sugar.RemoveTypes
  ( nonHoleTypes
  , holeResultTypes
  , successfulType
  , inferredTypes
  , types
  ) where

import Control.Lens.Operators
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens

nonHoleTypes :: Expression name m a -> Expression name m a
nonHoleTypes =
  successfulType . (innerLayer %~ nonHoleTypes)
  & (Lens.outside . Lens.filtered . Lens.has) (rBody . _BodyHole) .~
    (innerLayer . innerLayer %~ nonHoleTypes)
  where
    innerLayer = rBody . Lens.traversed

holeResultTypes :: Expression name m a -> Expression name m a
holeResultTypes =
  successfulType .
  ( rBody %~
    ( (Lens.traversed %~ types)
      & Lens.outside _BodyHole .~
        BodyHole . (Lens.traversed . rBody . Lens.traversed %~ types)
    )
  )

successfulType :: Expression name m a -> Expression name m a
successfulType = rPayload %~ plRemoveSuccessfulType

plRemoveSuccessfulType :: Payload name m a -> Payload name m a
plRemoveSuccessfulType =
  plInferredTypes . Lens.filtered (null . drop 1) .~ []

inferredTypes :: Expression name m a -> Expression name m a
inferredTypes = rPayload . plInferredTypes .~ []

types :: Expression name m a -> Expression name m a
types = fmap plRemoveSuccessfulType
