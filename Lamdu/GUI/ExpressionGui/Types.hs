{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
    ( ExpressionGui
    , SugarExpr
    , Payload(..)
        , plStoredEntityIds, plInjected, plNearestHoles, plShowAnnotation
    , emptyPayload
    , ShowAnnotation(..)
    , nextHolesBefore, markRedundantTypes
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import           Lamdu.Sugar.Names.Types (ExpressionN)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import           Lamdu.Sugar.RedundantTypes (redundantTypes)
import qualified Lamdu.Sugar.Types as Sugar

type ExpressionGui m = Layout (Transaction m)

data ShowAnnotation = ShowAnnotationInVerboseMode | DoNotShowAnnotation | ShowAnnotation

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plStoredEntityIds :: [Sugar.EntityId]
    , _plInjected :: [Bool]
    , _plNearestHoles :: NearestHoles
    , _plShowAnnotation :: ShowAnnotation
    }
Lens.makeLenses ''Payload

type SugarExpr m = ExpressionN m Payload

emptyPayload :: NearestHoles -> Payload
emptyPayload nearestHoles = Payload
    { _plStoredEntityIds = []
    , _plInjected = []
    , _plNearestHoles = nearestHoles
    , _plShowAnnotation = ShowAnnotationInVerboseMode
    }

nextHolesBefore :: Sugar.Expression name m Payload -> NearestHoles
nextHolesBefore val =
    node ^. Sugar.rPayload . Sugar.plData . plNearestHoles
    & if Lens.has (Sugar.rBody . Sugar._BodyHole) node
        then NearestHoles.next .~ Just (node ^. Sugar.rPayload . Sugar.plEntityId)
        else id
    where
        node = leftMostLeaf val

leftMostLeaf :: Sugar.Expression name m a -> Sugar.Expression name m a
leftMostLeaf val =
    case val ^.. Sugar.rBody . Lens.traversed of
    [] -> val
    (x:_) -> leftMostLeaf x

markRedundantTypes :: SugarExpr m -> SugarExpr m
markRedundantTypes v =
    v
    & redundantTypes         . showType .~ DoNotShowAnnotation
    & SugarLens.holePayloads . showType .~ ShowAnnotation
    & SugarLens.holeArgs     . showType .~ ShowAnnotation
    & Sugar.rPayload         . showType .~ ShowAnnotation
    where
        showType = Sugar.plData . plShowAnnotation
