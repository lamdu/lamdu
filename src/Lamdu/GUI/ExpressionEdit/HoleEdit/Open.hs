{-# LANGUAGE NoImplicitPrelude #-}
-- | The search area (search term + results) of an open/active hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchAreaGui
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeOpenSearchAreaGui ::
    Monad m =>
    EventMap (T m GuiState.Update) ->
    (Text -> Bool) -> View ->
    Sugar.Payload (T m) ExprGui.Payload ->
    Menu.OptionList (Menu.Option (ExprGuiM m) (T m)) ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeOpenSearchAreaGui searchTermEventMap allowedTerms typeView pl options =
    do
        vspace <- Annotation.annotationSpacer
        let annotation = vspace /-/ typeView
        (mPickMain, menu) <-
            Menu.make (annotation ^. Element.width) mNextEntry options
            <&> _2 . Lens.mapped . Widget.eventMapMaker . Lens.mapped %~ (searchTermEventMap <>)
        pickEventMap <-
            case mPickMain of
            Nothing -> SearchMenu.emptyPickEventMap
            Just pickMain -> Menu.makePickEventMap mNextEntry ?? pickMain
        mkHoverOptions <- Menu.hoverOptions
        let hoverMenu placement term =
                Hover.hoverInPlaceOf (mkHoverOptions placement annotation menu a) a
                where
                    a = Hover.anchor term
        SearchTerm.make (hidOpen widgetIds) allowedTerms
            <&> Align.tValue %~ Widget.weakerEvents pickEventMap
            <&> \searchTermWidget placement ->
                searchTermWidget <&> hoverMenu placement
    & Reader.local (Element.animIdPrefix .~ WidgetId.toAnimId (hidOpen widgetIds))
    & SearchMenu.assignCursor (hidOpen widgetIds) (options ^.. traverse . Menu.oId)
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        mNextEntry = pl ^. Sugar.plData . ExprGui.plNearestHoles . NearestHoles.next <&> WidgetIds.fromEntityId
