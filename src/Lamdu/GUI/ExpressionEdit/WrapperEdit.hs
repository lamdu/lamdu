{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.WrapperEdit
    ( make
    ) where

import           Data.Store.Transaction (Transaction)
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ExpressionN)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.Name (Name(..))
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified GUI.Momentu as Momentu
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

make ::
    Monad m =>
    Sugar.Wrapper (Name (T m)) (T m) (ExpressionN m ExprGui.Payload) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make wrapper pl =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        let mRemoveNextHoles
                | isSelected = ExprGui.plNearestHoles .~ NearestHoles.none
                | otherwise = id
        argGui <-
            wrapper
            & Sugar.wExpr . Sugar.rPayload . Sugar.plData %~ mRemoveNextHoles
            & makeArgEdit & GuiState.assignCursor myId innerId
        hover <- Hover.hover
        searchAreaGui <- SearchArea.make (wrapper ^. Sugar.wOptions) Nothing pl allowedWrapperSearchTerm
        let f layoutMode arg
                | isSelected
                || Widget.isFocused (arg ^. Align.tValue) =
                    arg & Align.tValue %~ Hover.hoverInPlaceOf options . Hover.anchor
                | otherwise =
                    arg
                    where
                        options =
                            [ hoverArgument /-/ (searchArea Menu.Below <&> hover)
                            , (searchArea Menu.Above <&> hover) /-/ hoverArgument
                            ]
                            <&> (^. Align.tValue)
                        hoverArgument =
                            render argGui
                            & Align.tValue %~ setFocalArea
                            & Align.tValue %~ Hover.anchor
                        searchArea p =
                            render (searchAreaGui p)
                            & hideIfInHole
                        render x = (x ^. Responsive.render) layoutMode
                        setFocalArea w
                            | isSelected =
                                w
                                & Widget.wState . Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .~
                                    [Rect 0 (w ^. Widget.wSize)]
                            | otherwise = w
        config <- Lens.view Config.config
        let enterEventMap =
                E.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
                (E.Doc ["Navigation", "Enter wrapper"]) (pure innerId)
        let leaveEventMap =
                E.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
                (E.Doc ["Navigation", "Go out to wrapper"]) (pure myId)
        let addFocusEvents
                | isSelected = (enterEventMap <>) . E.filterChars (`notElem` Chars.operator)
                | otherwise = (<> leaveEventMap)
        let unwrapEventMap =
                case wrapper ^. Sugar.wUnwrap of
                Sugar.UnwrapTypeMismatch -> mempty
                Sugar.UnwrapAction unwrap ->
                    unwrap <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor
                        (Config.delKeys config <> Config.holeUnwrapKeys (Config.hole config))
                        (E.Doc ["Edit", "Unwrap"])
        ExprEventMap.add ExprEventMap.defaultOptions pl
            <*> (maybeAddAnnotationPl pl ?? argGui <&> Responsive.render . Lens.imapped %@~ f)
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ addFocusEvents
            <&> Widget.widget %~ Widget.weakerEvents unwrapEventMap
    where
        innerId = wrapper ^. Sugar.wExpr . Sugar.rPayload & WidgetIds.fromExprPayload
        myId = WidgetIds.fromExprPayload pl
        hideIfInHole x
            | ExprGui.isHoleResult pl =
                x
                & Element.setLayers .~ mempty
                & Element.size .~ 0
            | otherwise = x

makeArgEdit ::
    Monad m =>
    Sugar.Wrapper (Name (T m)) (T m) (ExpressionN m ExprGui.Payload) ->
    ExprGuiM m (ExpressionGui m)
makeArgEdit wrapper =
    do
        theme <- Lens.view Theme.theme
        let frameColor =
                theme &
                case wrapper ^. Sugar.wUnwrap of
                Sugar.UnwrapAction {} -> Theme.typeIndicatorMatchColor
                Sugar.UnwrapTypeMismatch {} -> Theme.typeIndicatorErrorColor
        let frameWidth = Theme.typeIndicatorFrameWidth theme <&> realToFrac
        argGui <- ExprGuiM.makeSubexpression (wrapper ^. Sugar.wExpr)
        Momentu.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Momentu.pad (frameWidth & _2 .~ 0) argGui

allowedWrapperSearchTerm :: Text -> Bool
allowedWrapperSearchTerm searchTerm =
    SearchArea.allowedSearchTermCommon searchTerm || isGetField searchTerm
    where
        isGetField t =
            case Text.uncons t of
            Just (c, rest) -> c == '.' && Text.all Char.isAlphaNum rest
            Nothing -> False
