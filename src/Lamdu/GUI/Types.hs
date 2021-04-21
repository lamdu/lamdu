module Lamdu.GUI.Types
    ( Expr, Body, Payload, Top
    , mParensId
    ) where

import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type Expr t i o = Sugar.Expr t (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o
type Body t i o = Sugar.Body t (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o

type Payload i o = Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) o

type Top t i o = t (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o (Payload i o)

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: Sugar.Payload v o -> Maybe AnimId
mParensId pl
    | pl ^. Sugar.plParenInfo . Sugar.piNeedParens =
          pl & WidgetIds.fromExprPayload & WidgetId.toAnimId & Just
    | otherwise = Nothing
