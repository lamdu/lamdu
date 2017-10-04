{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (transaction)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Argument as Wrapper
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeWrapper ::
    Monad m =>
    Sugar.Payload (T m) ExprGuiT.Payload -> Widget.Id -> Sugar.HoleArg (T m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeWrapper pl openHoleId holeArg =
    do
        (wrapper, holePicker) <- Wrapper.make openHoleId holeArg & ExprGuiM.listenResultPicker
        exprEventMap <- ExprEventMap.make pl holePicker
        E.weakerEvents exprEventMap wrapper & return

assignHoleCursor ::
    Monad m => WidgetIds -> ExprGuiM m a -> ExprGuiM m a
assignHoleCursor widgetIds =
    Widget.assignCursor hidHole hidOpen .
    Widget.assignCursor (WidgetIds.notDelegatingId hidHole) hidClosedSearchArea
    where
        WidgetIds{hidHole,hidOpen,hidClosedSearchArea} = widgetIds

makeHoleWithWrapper ::
    (Functor f, Monad m) =>
    (Menu.Placement -> ExpressionGui f) -> Sugar.Payload (T m) ExprGuiT.Payload -> ExpressionGui f ->
    ExprGuiM m (ExpressionGui f)
makeHoleWithWrapper searchAreaGui pl wrapperGui =
    do
        unfocusedWrapperGui <-
            ExpressionGui.maybeAddAnnotationPl pl ?? wrapperGui
        isSelected <- Widget.isSubCursor ?? hidHole widgetIds
        hover <- Hover.hover
        let f layoutMode wrapper =
                case isSelected || Widget.isFocused (wrapper ^. Align.tValue) of
                True ->
                    wrapper & Align.tValue %~ Hover.hoverInPlaceOf options . Hover.anchor
                    where
                        options =
                            [ hoverWrapper /-/ (searchArea Menu.Below <&> hover)
                            , (searchArea Menu.Above <&> hover) /-/ hoverWrapper
                            ]
                            <&> (^. Align.tValue)
                        hoverWrapper = render wrapperGui & Align.tValue %~ Hover.anchor
                        searchArea p =
                            render (searchAreaGui p)
                            & hideIfInHole
                        render x = (x ^. Responsive.render) layoutMode
                False -> wrapper
        unfocusedWrapperGui
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
        stateProp <-
            HoleState.assocStateRef (hole ^. Sugar.holeActions . Sugar.holeUUID)
            ^. Transaction.mkProperty & transaction

        searchAreaGui <-
            SearchArea.makeStdWrapped pl
            HoleInfo
            { hiEntityId = pl ^. Sugar.plEntityId
            , hiState = stateProp
            , hiInferredType = pl ^. Sugar.plAnnotation . Sugar.aInferredType
            , hiHole = hole
            , hiIds = widgetIds
            , hiNearestHoles = pl ^. Sugar.plData . ExprGuiT.plNearestHoles
            , hiMinOpPrec = pl ^. Sugar.plData . ExprGuiT.plMinOpPrec
            }

        case hole ^. Sugar.holeKind of
            Sugar.WrapperHole arg ->
                makeWrapper pl (hidOpenSearchTerm widgetIds) arg
                >>= makeHoleWithWrapper searchAreaGui pl
            Sugar.LeafHole{} -> return (searchAreaGui Menu.AnyPlace)
    & assignHoleCursor widgetIds
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId (hidHole widgetIds))
    where
        widgetIds = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
