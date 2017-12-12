{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Argument as Argument
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeArgument ::
    Monad m =>
    Sugar.Payload (T m) ExprGuiT.Payload -> Widget.Id -> Sugar.HoleArg (T m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeArgument pl openHoleId holeArg =
    do
        (wrapper, holePicker) <- Argument.make openHoleId holeArg & ExprGuiM.listenResultPicker
        exprEventMap <- ExprEventMap.make pl holePicker
        E.weakerEvents exprEventMap wrapper & return

assignHoleCursor ::
    Monad m => WidgetIds -> ExprGuiM m a -> ExprGuiM m a
assignHoleCursor widgetIds =
    GuiState.assignCursor (hidHole widgetIds) (hidOpen widgetIds)

makeHoleWithArgument ::
    (Functor f, Monad m) =>
    (Menu.Placement -> ExpressionGui f) -> Sugar.Payload (T m) ExprGuiT.Payload -> ExpressionGui f ->
    ExprGuiM m (ExpressionGui f)
makeHoleWithArgument searchAreaGui pl wrapperGui =
    do
        unfocusedArgumentGui <- maybeAddAnnotationPl pl ?? wrapperGui
        isSelected <- GuiState.isSubCursor ?? hidHole widgetIds
        hover <- Hover.hover
        let f layoutMode wrapper =
                case isSelected || Widget.isFocused (wrapper ^. Align.tValue) of
                True ->
                    wrapper & Align.tValue %~ Hover.hoverInPlaceOf options . Hover.anchor
                    where
                        options =
                            [ hoverArgument /-/ (searchArea Menu.Below <&> hover)
                            , (searchArea Menu.Above <&> hover) /-/ hoverArgument
                            ]
                            <&> (^. Align.tValue)
                        hoverArgument = render wrapperGui & Align.tValue %~ Hover.anchor
                        searchArea p =
                            render (searchAreaGui p)
                            & hideIfInHole
                        render x = (x ^. Responsive.render) layoutMode
                False -> wrapper
        unfocusedArgumentGui
            & Responsive.render . Lens.imapped %@~ f
            & pure
    where
        widgetIds = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
        hideIfInHole x
            | ExprGuiT.isHoleResult pl =
                x
                & Element.setLayers .~ mempty
                & Element.size .~ 0
            | otherwise = x

make ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression (Name (T m)) (T m) ()) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload (T m) ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make hole pl =
    do
        searchAreaGui <- SearchArea.makeStdWrapped hole pl widgetIds
        case hole ^. Sugar.holeKind of
            Sugar.WrapperHole arg ->
                makeArgument pl (hidOpenSearchTerm widgetIds) arg
                >>= makeHoleWithArgument searchAreaGui pl
            Sugar.LeafHole{} -> return (searchAreaGui Menu.AnyPlace)
    & assignHoleCursor widgetIds
    where
        widgetIds = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
