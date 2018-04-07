{-# LANGUAGE OverloadedStrings #-}
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
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    (Monad i, Monad o) =>
    Sugar.Fragment (Name o) i o
    (ExpressionN i o ExprGui.Payload) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (ExpressionGui o)
make fragment pl =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        let mRemoveNextHoles
                | isSelected = ExprGui.plNearestHoles .~ NearestHoles.none
                | otherwise = id
        config <- Lens.view Config.config
        fragmentExprGui <-
            fragment
            & Sugar.fExpr . Sugar.rPayload . Sugar.plData %~ mRemoveNextHoles
            & makeFragmentExprEdit & GuiState.assignCursor myId innerId
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
                        searchArea p
                            | ExprGui.isHoleResult pl = Element.empty
                            | otherwise = render (searchAreaGui p)
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
                        (Config.delKeys config <> config ^. Config.attachKeys)
                        (E.Doc ["Edit", "Attach"])
        ExprEventMap.add ExprEventMap.defaultOptions pl
            <*> (maybeAddAnnotationPl pl ?? fragmentExprGui <&> Responsive.render . Lens.imapped %@~ f)
            <&> Widget.widget %~ Widget.weakerEvents attachEventMap
    where
        innerId = fragment ^. Sugar.fExpr . Sugar.rPayload & WidgetIds.fromExprPayload
        myId = WidgetIds.fromExprPayload pl

makeFragmentExprEdit ::
    (Monad i, Functor o) =>
    Sugar.Fragment (Name o) i o
    (ExpressionN i o ExprGui.Payload) ->
    ExprGuiM i o (ExpressionGui o)
makeFragmentExprEdit fragment =
    do
        theme <- Lens.view Theme.theme
        let frameColor =
                theme ^.
                case fragment ^. Sugar.fAttach of
                Sugar.AttachAction {} -> Theme.typeIndicatorMatchColor
                Sugar.AttachTypeMismatch {} -> Theme.typeIndicatorErrorColor
        let frameWidth = theme ^. Theme.typeIndicatorFrameWidth
        fragmentExprGui <- ExprGuiM.makeSubexpression (fragment ^. Sugar.fExpr)
        Momentu.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Momentu.pad (frameWidth & _2 .~ 0) fragmentExprGui
