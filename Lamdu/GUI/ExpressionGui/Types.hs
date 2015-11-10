{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
    ( ExpressionGui
    , SugarExpr
    , Payload(..)
        , plStoredEntityIds, plInjected, plNearestHoles, plShowAnnotation
    , emptyPayload
    , EvalModeShow(..)
    , ShowAnnotation(..), showTypeWhenMissing, showInTypeMode, showInEvalMode
      , showAnnotationWhenVerbose
      , neverShowAnnotations, alwaysShowAnnotations
    , nextHolesBefore
    , plOfHoleResult
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import           Lamdu.Sugar.Names.Types (ExpressionN)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type ExpressionGui m = Layout (Transaction m)

data EvalModeShow = EvalModeShowNothing | EvalModeShowType | EvalModeShowEval
    deriving (Eq, Ord, Show)

data ShowAnnotation = ShowAnnotation
    { _showTypeWhenMissing :: Bool -- concise-mode or eval-mode without val
    , _showInTypeMode :: Bool
    , _showInEvalMode :: EvalModeShow
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''ShowAnnotation

showAnnotationWhenVerbose :: ShowAnnotation
showAnnotationWhenVerbose = ShowAnnotation
    { _showTypeWhenMissing = False
    , _showInTypeMode = True
    , _showInEvalMode = EvalModeShowEval
    }

neverShowAnnotations :: ShowAnnotation
neverShowAnnotations = ShowAnnotation False False EvalModeShowNothing

alwaysShowAnnotations :: ShowAnnotation
alwaysShowAnnotations = ShowAnnotation True True EvalModeShowEval

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plStoredEntityIds :: [Sugar.EntityId]
    , _plInjected :: [Bool]
    , _plNearestHoles :: NearestHoles
    , _plShowAnnotation :: ShowAnnotation
    }
Lens.makeLenses ''Payload

plOfHoleResult :: Sugar.Payload m Payload -> Bool
plOfHoleResult =
    Lens.nullOf (Sugar.plData . plStoredEntityIds . Lens.traversed)

type SugarExpr m = ExpressionN m Payload

emptyPayload :: NearestHoles -> Payload
emptyPayload nearestHoles = Payload
    { _plStoredEntityIds = []
    , _plInjected = []
    , _plNearestHoles = nearestHoles
    , _plShowAnnotation = showAnnotationWhenVerbose
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
