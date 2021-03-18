module Lamdu.GUI.Expr.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o)
make pl =
    do
        litEventMap <- ExprEventMap.makeLiteralEventMap ?? pl ^. Sugar.plActions . Sugar.setToLiteral
        (ExprEventMap.add options pl <&> (Align.tValue %~))
            <*> ((Widget.makeFocusableView ?? WidgetIds.fromExprPayload pl <&> fmap) <*> Label.make "_")
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (litEventMap <>)
            <&> Responsive.fromWithTextPos
    where
        options =
            ExprEventMap.defaultOptions
            { ExprEventMap.addOperatorSetHoleState = Just (pl ^. Sugar.plEntityId)
            }
