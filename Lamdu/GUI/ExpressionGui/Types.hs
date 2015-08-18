{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
    ( ExpressionGui
    , SugarExpr
    , Payload(..)
        , plStoredEntityIds, plInjected, plNearestHoles, plShowAnnotation
    , emptyPayload
    , EvalModeShow(..)
    , ShowAnnotation(..), showAnnotationWhenVerbose
      , neverShowAnnotations, alwaysShowAnnotations
    , nextHolesBefore, markAnnotationsToDisplay
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

markAnnotationsToDisplay :: SugarExpr m -> SugarExpr m
markAnnotationsToDisplay v =
    v
    & SugarLens.subExprsOf Sugar._BodyToNom   . showAnn . showInEvalMode .~ EvalModeShowNothing
    & SugarLens.subExprsOf Sugar._BodyFromNom . showAnn . showInEvalMode .~ EvalModeShowNothing
    & SugarLens.payloadsOf Sugar._BodyInject  . showAnn . showInEvalMode .~ EvalModeShowNothing
    & redundantTypes                          . showAnn %~
      (showInTypeMode .~ False) .
      (showInEvalMode .~ EvalModeShowNothing) -- TODO: This makes little sense
    & SugarLens.holePayloads                  . showAnn %~
      (showTypeWhenMissing .~ True) .
      (showInEvalMode .~ EvalModeShowType)
    & SugarLens.holeArgs                      . showAnn %~
      (showTypeWhenMissing .~ True) .
      (showInEvalMode %~ don'tShowNothing)
    where
        don'tShowNothing EvalModeShowNothing = EvalModeShowType
        don'tShowNothing x = x
        showAnn = Sugar.plData . plShowAnnotation
