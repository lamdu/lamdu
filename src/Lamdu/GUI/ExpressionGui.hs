{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui
    , SugarExpr
    , Payload(..)
        , plHiddenEntityIds, plNearestHoles, plNeedParens, plMinOpPrec
    , nextHolesBefore
    , adhocPayload
    , mParensId
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type ExpressionGui m = Responsive (m GuiState.Update)

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plHiddenEntityIds :: [Sugar.EntityId]
    , _plNearestHoles :: NearestHoles
    , _plNeedParens :: Bool
    , _plMinOpPrec :: Int
    } deriving (Generic, Eq, Show)
Lens.makeLenses ''Payload

type SugarExpr i o =
    Sugar.Expression (Name o) i o (Sugar.Payload (Name o) i o Payload)

nextHolesBefore ::
    Sugar.Expression name0 i0 o0 (Sugar.Payload name1 i1 o1 Payload) ->
    NearestHoles
nextHolesBefore val =
    node ^. Sugar.annotation . Sugar.plData . plNearestHoles
    & if Lens.has (Sugar.body . SugarLens.bodyUnfinished) node
        then NearestHoles.next ?~ node ^. Sugar.annotation . Sugar.plEntityId
        else id
    where
        node = SugarLens.leftMostLeaf val

-- | Used to create a sugar expr payload when the original sugar
-- expression contains a fake unit () payload (in LabeledApply and InjectNullary)
adhocPayload :: NearestHoles -> Payload
adhocPayload nearestHoles = Payload
    { _plHiddenEntityIds = []
    , _plNearestHoles = nearestHoles
    , _plNeedParens = False
    , _plMinOpPrec = 13 -- TODO: Export Parens.applyPrec?
    }

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: Sugar.Payload name i o Payload -> Maybe AnimId
mParensId pl
    | pl ^. Sugar.plData . plNeedParens =
          WidgetIds.fromExprPayload pl & WidgetId.toAnimId & Just
    | otherwise = Nothing
