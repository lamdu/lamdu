{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Payload
    ( SugarExpr
    , Payload(..), plHiddenEntityIds, plNeedParens, plMinOpPrec
    , mParensId
    ) where

import qualified Control.Lens.Extended as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

-- GUI input payload on sugar exprs
data Payload = Payload
    { _plHiddenEntityIds :: [Sugar.EntityId]
    , _plNeedParens :: Bool
    , _plMinOpPrec :: Int
    } deriving (Generic, Eq, Show)
Lens.makeLenses ''Payload

type SugarExpr i o =
    Sugar.Expression Name i o (Sugar.Payload Name i o Payload)

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: Sugar.Payload name i o Payload -> Maybe AnimId
mParensId pl
    | pl ^. Sugar.plData . plNeedParens =
          WidgetIds.fromExprPayload pl & WidgetId.toAnimId & Just
    | otherwise = Nothing
