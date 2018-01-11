{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui
    , SugarExpr
    , Payload(..)
        , plStoredEntityIds, plNearestHoles, plShowAnnotation, plNeedParens
        , plMinOpPrec
    , EvalModeShow(..)
    , FuncApplyLimit(..)
    , ShowAnnotation(..), showExpanded, showInTypeMode, showInEvalMode
      , funcApplyLimit
      , showAnnotationWhenVerbose
      , neverShowAnnotations, alwaysShowAnnotations
    , nextHolesBefore
    , isHoleResult
    , ExpressionN
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Name (Name)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

type ExpressionGui m = Responsive (T m GuiState.Update)

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
    , _plNeedParens :: Bool
    , _plMinOpPrec :: Int
    }
Lens.makeLenses ''Payload

isHoleResult :: Sugar.Payload f Payload -> Bool
isHoleResult =
    Lens.nullOf (Sugar.plData . plStoredEntityIds . Lens.traversed)

type ExpressionN m a = Sugar.Expression (Name (T m)) (T m) a
type SugarExpr m = ExpressionN m Payload

nextHolesBefore :: Sugar.Expression name m Payload -> NearestHoles
nextHolesBefore val =
    node ^. Sugar.rPayload . Sugar.plData . plNearestHoles
    & if Lens.has (Sugar.rBody . SugarLens.bodyHoleOrWrapper) node
        then NearestHoles.next .~ Just (node ^. Sugar.rPayload . Sugar.plEntityId)
        else id
    where
        node = SugarLens.leftMostLeaf val
