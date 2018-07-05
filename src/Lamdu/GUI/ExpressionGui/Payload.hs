{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Payload
    ( SugarExpr
    , Payload(..)
        , plHiddenEntityIds, plNearestHoles, plNeedParens, plMinOpPrec
    , nextHolesBefore
    , mParensId
    ) where

import qualified Control.Lens.Extended as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

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
nextHolesBefore val
    | Lens.has SugarLens.bodyUnfinished bod =
        plNearest & NearestHoles.next ?~ pl ^. Sugar.plEntityId
    | otherwise = plNearest
    where
        (bod, pl) = val ^?! SugarLens.exprPayloads . Lens.filteredByIndex SugarLens._OfExpr . Lens.withIndex
        plNearest = pl ^. Sugar.plData . plNearestHoles

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: Sugar.Payload name i o Payload -> Maybe AnimId
mParensId pl
    | pl ^. Sugar.plData . plNeedParens =
          WidgetIds.fromExprPayload pl & WidgetId.toAnimId & Just
    | otherwise = Nothing
