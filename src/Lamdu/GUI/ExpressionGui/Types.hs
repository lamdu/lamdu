{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
    ( ExpressionGui
    , SugarExpr
    , Payload(..)
        , plStoredEntityIds, plNearestHoles, plShowAnnotation
    , EvalModeShow(..)
    , FuncApplyLimit(..)
    , ShowAnnotation(..), showExpanded, showInTypeMode, showInEvalMode
      , funcApplyLimit
      , showAnnotationWhenVerbose
      , neverShowAnnotations, alwaysShowAnnotations
    , nextHolesBefore
    , isHoleResult
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Responsive (Responsive(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (ExpressionN)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type ExpressionGui m = Responsive (Transaction m Widget.EventResult)

data EvalModeShow = EvalModeShowNothing | EvalModeShowType | EvalModeShowEval
    deriving (Eq, Ord, Show)

-- This is only relevant for function subexprs, and means their
-- parameter can only ever get one scope per parent scope id, meaning
-- we may avoid showing their scope nav altogether.
data FuncApplyLimit = UnlimitedFuncApply | AtMostOneFuncApply
    deriving (Eq, Ord, Show)

data ShowAnnotation = ShowAnnotation
    { -- showExpanded means we:
      -- A) Show even in concise-mode & eval-mode without val
      -- B) Do not shrink the annotation to fit
      _showExpanded :: Bool
    , _showInTypeMode :: Bool
    , _showInEvalMode :: EvalModeShow
    , _funcApplyLimit :: FuncApplyLimit
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''ShowAnnotation

showAnnotationWhenVerbose :: ShowAnnotation
showAnnotationWhenVerbose = ShowAnnotation
    { _showExpanded = False
    , _showInTypeMode = True
    , _showInEvalMode = EvalModeShowEval
    , _funcApplyLimit = UnlimitedFuncApply
    }

neverShowAnnotations :: ShowAnnotation
neverShowAnnotations = ShowAnnotation False False EvalModeShowNothing UnlimitedFuncApply

alwaysShowAnnotations :: ShowAnnotation
alwaysShowAnnotations = ShowAnnotation True True EvalModeShowEval UnlimitedFuncApply

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plStoredEntityIds :: [Sugar.EntityId]
    , _plNearestHoles :: NearestHoles
    , _plShowAnnotation :: ShowAnnotation
    }
Lens.makeLenses ''Payload

isHoleResult :: Sugar.Payload m Payload -> Bool
isHoleResult =
    Lens.nullOf (Sugar.plData . plStoredEntityIds . Lens.traversed)

type SugarExpr m = ExpressionN m Payload

nextHolesBefore :: Sugar.Expression name m Payload -> NearestHoles
nextHolesBefore val =
    node ^. Sugar.rPayload . Sugar.plData . plNearestHoles
    & if Lens.has (Sugar.rBody . Sugar._BodyHole) node
        then NearestHoles.next .~ Just (node ^. Sugar.rPayload . Sugar.plEntityId)
        else id
    where
        node = SugarLens.leftMostLeaf val
