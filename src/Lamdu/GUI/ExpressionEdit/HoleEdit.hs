{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
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
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Argument as Argument
import           Lamdu.GUI.ExpressionEdit.HoleEdit.LiteralText (makeLiteralTextEventMap)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui.Wrap (addActions)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeHoleWithArgument ::
    (Monad f, Monad m) =>
    (Menu.Placement -> ExpressionGui f) ->
    Sugar.HoleArg (T f) expr -> Sugar.Payload (T m) ExprGui.Payload ->
    ExpressionGui f ->
    ExprGuiM m (ExpressionGui f)
makeHoleWithArgument searchAreaGui arg pl wrapperGui =
    do
        isSelected <- GuiState.isSubCursor ?? hidHole widgetIds
        hover <- Hover.hover
        let f layoutMode wrapper
                | isSelected
                || Widget.isFocused (wrapper ^. Align.tValue) =
                    wrapper & Align.tValue %~ Hover.hoverInPlaceOf options . Hover.anchor
                | otherwise = wrapper
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
        config <- Lens.view Config.config
        let unwrapEventMap =
                case arg ^. Sugar.haUnwrap of
                Sugar.UnwrapTypeMismatch -> mempty
                Sugar.UnwrapAction unwrap ->
                    unwrap <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor
                        (Config.delKeys config <> Config.holeUnwrapKeys (Config.hole config))
                        (E.Doc ["Edit", "Unwrap"])
        maybeAddAnnotationPl pl ?? wrapperGui
            <&> Responsive.render . Lens.imapped %@~ f
            <&> Widget.widget %~ Widget.weakerEvents unwrapEventMap
    where
        widgetIds = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
        hideIfInHole x
            | ExprGui.isHoleResult pl =
                x
                & Element.setLayers .~ mempty
                & Element.size .~ 0
            | otherwise = x

make ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression (Name (T m)) (T m) ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make hole pl =
    do
        searchAreaGui <- SearchArea.make hole pl
        case hole ^. Sugar.holeKind of
            Sugar.WrapperHole arg ->
                Argument.make arg
                >>= makeHoleWithArgument searchAreaGui arg pl
            Sugar.LeafHole{} ->
                makeLiteralTextEventMap (hole ^. Sugar.holeKind) widgetIds
                <&>
                \txtEventMap ->
                searchAreaGui Menu.AnyPlace & Widget.weakerEvents txtEventMap
    & GuiState.assignCursor (hidHole widgetIds) (hidOpen widgetIds)
    & addActions options pl
    where
        widgetIds = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
        options = ExprEventMap.defaultOptions { ExprEventMap.addOperatorDontWrap = True }
