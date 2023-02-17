module Lamdu.GUI.Types
    ( Expr, Body, Payload, Top
    , mParensId
    ) where

import qualified GUI.Momentu.Element.Id as ElementId
import           GUI.Momentu.Element.Id (ElemId)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type AnnEval i = Sugar.Annotation (Sugar.EvaluationScopes Name i) Name

type Expr t i o = Sugar.Expr t (AnnEval i) Name i o
type Body t i o = Sugar.Body t (AnnEval i) Name i o

type Payload i o = Sugar.Payload (AnnEval i) o

type Top t i o = t (AnnEval i) Name i o (Payload i o)

-- | Just myId or Nothing depending on whether parens are needed
mParensId :: Sugar.Payload v o -> Maybe ElemId
mParensId pl
    | pl ^. Sugar.plParenInfo . Sugar.piNeedParens =
          pl & WidgetIds.fromExprPayload & ElementId.asElemId & Just
    | otherwise = Nothing
