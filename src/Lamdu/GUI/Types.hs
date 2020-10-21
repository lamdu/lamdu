{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Types
    ( Expr, Body, Payload, Top
    , GuiPayload(..), plHiddenEntityIds, plParenInfo
    , mParensId
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

-- TODO: This is not specific to GUI at all, can move to Sugar.Types?
-- GUI input payload on sugar exprs
data GuiPayload = GuiPayload
    { _plParenInfo :: !Sugar.ParenInfo
    , _plHiddenEntityIds :: [Sugar.EntityId]
    } deriving (Generic, Eq, Show)
Lens.makeLenses ''GuiPayload

type Expr t i o = Sugar.Expr t (Sugar.EvaluationScopes Name i) Name i o GuiPayload
type Body t i o = Sugar.Body t (Sugar.EvaluationScopes Name i) Name i o GuiPayload

type Payload i o = (Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o, GuiPayload)

type Top t i o = t (Sugar.EvaluationScopes Name i) Name i o (Payload i o)

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: (Sugar.Payload v name i o, GuiPayload) -> Maybe AnimId
mParensId pl
    | pl ^. _2 . plParenInfo . Sugar.piNeedParens =
          pl ^. _1 & WidgetIds.fromExprPayload & WidgetId.toAnimId & Just
    | otherwise = Nothing
