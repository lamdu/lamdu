{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Payload
    ( Payload(..), plHiddenEntityIds, plParenInfo
    , mParensId
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

-- TODO: This is not specific to GUI at all, can move to Sugar.Types?
-- GUI input payload on sugar exprs
data Payload = Payload
    { _plParenInfo :: !Sugar.ParenInfo
    , _plHiddenEntityIds :: [Sugar.EntityId]
    } deriving (Generic, Eq, Show)
Lens.makeLenses ''Payload

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: (Sugar.Payload v name i o, Payload) -> Maybe AnimId
mParensId pl
    | pl ^. _2 . plParenInfo . Sugar.piNeedParens =
          pl ^. _1 & WidgetIds.fromExprPayload & WidgetId.toAnimId & Just
    | otherwise = Nothing
