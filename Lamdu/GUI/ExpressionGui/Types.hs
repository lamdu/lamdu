{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
    ( ExpressionGuiM(..)
    , ExpressionGui, toLayout
      , egWidget, egAbsWidget, egAlignment, egLayout
      , fromValueWidget, fromLayout
    , LayoutParams(..), layoutMode, layoutContext
    , LayoutMode(..), _LayoutNarrow, _LayoutWide
      , modeWidths
    , LayoutDisambiguationContext(..)
    , SugarExpr
    , Payload(..)
        , plStoredEntityIds, plInjected, plNearestHoles, plShowAnnotation
    , emptyPayload
    , EvalModeShow(..)
    , FuncApplyLimit(..)
    , ShowAnnotation(..), showExpanded, showInTypeMode, showInEvalMode
      , funcApplyLimit
      , showAnnotationWhenVerbose
      , neverShowAnnotations, alwaysShowAnnotations
    , nextHolesBefore
    , plOfHoleResult
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (ExpressionN)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction
data LayoutMode
    = LayoutNarrow Widget.R -- ^ limited by the given
    | LayoutWide -- ^ no limit on width
Lens.makePrisms ''LayoutMode

-- The relevant context for knowing whether parenthesis/indentation is needed
data LayoutDisambiguationContext
    = LayoutClear
    | LayoutHorizontal
    | LayoutVertical

data LayoutParams = LayoutParams
    { _layoutMode :: LayoutMode
    , _layoutContext :: LayoutDisambiguationContext
    }
Lens.makeLenses ''LayoutParams

modeWidths :: Lens.Traversal' LayoutMode Widget.R
modeWidths _ LayoutWide = pure LayoutWide
modeWidths f (LayoutNarrow limit) = f limit <&> LayoutNarrow

newtype ExpressionGuiM m = ExpressionGui
    { _toLayout :: LayoutParams -> Layout (m Widget.EventResult)
    }
Lens.makeLenses ''ExpressionGuiM

type ExpressionGui m = ExpressionGuiM (T m)

fromLayout :: Layout (m Widget.EventResult) -> ExpressionGuiM m
fromLayout = ExpressionGui . const

fromValueWidget :: Widget (m Widget.EventResult) -> ExpressionGuiM m
fromValueWidget = fromLayout . Layout.fromCenteredWidget

{-# INLINE egLayout #-}
egLayout ::
    Lens.Setter
    (ExpressionGuiM m)
    (ExpressionGuiM n)
    (Layout (m Widget.EventResult))
    (Layout (n Widget.EventResult))
egLayout = toLayout . Lens.mapped

{-# INLINE egWidget #-}
egWidget ::
    Lens.Setter
    (ExpressionGuiM m)
    (ExpressionGuiM n)
    (Widget (m Widget.EventResult))
    (Widget (n Widget.EventResult))
egWidget = egLayout . Layout.widget

{-# INLINE egAbsWidget #-}
egAbsWidget ::
    Lens.Setter
    (ExpressionGuiM m)
    (ExpressionGuiM n)
    (Widget (m Widget.EventResult))
    (Widget (n Widget.EventResult))
egAbsWidget = egLayout . Layout.absAlignedWidget . _2

{-# INLINE egAlignment #-}
egAlignment :: Lens.Setter' (ExpressionGui m) Layout.Alignment
egAlignment = egLayout . Layout.alignment

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
        node = SugarLens.leftMostLeaf val
