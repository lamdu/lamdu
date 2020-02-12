{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Payload
    ( SugarExpr
    , Payload(..), plHiddenEntityIds, plParenInfo
    , module Lamdu.Sugar.Parens
    , mParensId
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Parens (ParenInfo(..), piMinOpPrec, piNeedParens)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plParenInfo :: !ParenInfo
    , _plHiddenEntityIds :: [Sugar.EntityId]
    } deriving (Generic, Eq, Show)
Lens.makeLenses ''Payload

type SugarExpr i o =
    Sugar.Expression Name i o (Sugar.Payload Name i o Payload)

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: Sugar.Payload name i o Payload -> Maybe AnimId
mParensId pl
    | pl ^. Sugar.plData . plParenInfo . piNeedParens =
          WidgetIds.fromExprPayload pl & WidgetId.toAnimId & Just
    | otherwise = Nothing
