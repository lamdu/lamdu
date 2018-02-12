{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.FragmentEdit
    ( make
    ) where

import qualified Control.Lens as Lens
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
import           Lamdu.GUI.ExpressionEdit.HoleEdit.AllowedSearchTerm (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

make ::
    Monad m =>
    Sugar.Fragment (Name (T m)) (T m) (ExpressionN m ExprGui.Payload) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make fragment pl =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        let mRemoveNextHoles
                | isSelected = ExprGui.plNearestHoles .~ NearestHoles.none
                | otherwise = id
        config <- Lens.view Config.config
        let enterEventMap =
                E.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
                (E.Doc ["Navigation", "Enter fragment"]) (pure innerId)
        let leaveEventMap =
                E.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
                (E.Doc ["Navigation", "Leave fragment"]) (pure myId)
        let addFocusEvents
                | isSelected = (enterEventMap <>) . E.filterChars (`notElem` Chars.operator)
                | otherwise = (<> leaveEventMap)
        fragmentExprGui <-
            fragment
            & Sugar.fExpr . Sugar.rPayload . Sugar.plData %~ mRemoveNextHoles
            & makeFragmentExprEdit & GuiState.assignCursor myId innerId
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ addFocusEvents
        hover <- Hover.hover
        searchAreaGui <-
            SearchArea.make (fragment ^. Sugar.fOptions) Nothing pl allowedFragmentSearchTerm
        let f layoutMode fragmentExpr
                | isSelected
                || Widget.isFocused (fragmentExpr ^. Align.tValue) =
                    fragmentExpr & Align.tValue %~ Hover.hoverInPlaceOf options . Hover.anchor
                | otherwise =
                    fragmentExpr
                    where
                        options =
                            [ hoverFragmentExpr /-/ (searchArea Menu.Below <&> hover)
                            , (searchArea Menu.Above <&> hover) /-/ hoverFragmentExpr
                            ]
                            <&> (^. Align.tValue)
                        hoverFragmentExpr =
                            render fragmentExprGui
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
        let attachEventMap =
                case fragment ^. Sugar.fAttach of
                Sugar.AttachTypeMismatch -> mempty
                Sugar.AttachAction attach ->
                    attach <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor
                        (Config.delKeys config <> Config.attachKeys config)
                        (E.Doc ["Edit", "Attach"])
        ExprEventMap.add ExprEventMap.defaultOptions pl
            <*> (maybeAddAnnotationPl pl ?? fragmentExprGui <&> Responsive.render . Lens.imapped %@~ f)
            <&> Widget.widget %~ Widget.weakerEvents attachEventMap
    where
        innerId = fragment ^. Sugar.fExpr . Sugar.rPayload & WidgetIds.fromExprPayload
        myId = WidgetIds.fromExprPayload pl
        hideIfInHole x
            | ExprGui.isHoleResult pl =
                x
                & Element.setLayers .~ mempty
                & Element.size .~ 0
            | otherwise = x

makeFragmentExprEdit ::
    Monad m =>
    Sugar.Fragment (Name (T m)) (T m) (ExpressionN m ExprGui.Payload) ->
    ExprGuiM m (ExpressionGui m)
makeFragmentExprEdit fragment =
    do
        theme <- Lens.view Theme.theme
        let frameColor =
                theme &
                case fragment ^. Sugar.fAttach of
                Sugar.AttachAction {} -> Theme.typeIndicatorMatchColor
                Sugar.AttachTypeMismatch {} -> Theme.typeIndicatorErrorColor
        let frameWidth = Theme.typeIndicatorFrameWidth theme
        fragmentExprGui <- ExprGuiM.makeSubexpression (fragment ^. Sugar.fExpr)
        Momentu.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Momentu.pad (frameWidth & _2 .~ 0) fragmentExprGui
