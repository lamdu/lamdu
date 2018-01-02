{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}
-- | The search area (search term + results) of an open/active hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchAreaGui
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultsList(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as HoleResults
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget as ResultWidget
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data ResultGroup m = ResultGroup
    { rgOption :: !(Menu.Option (ExprGuiM m) (T m GuiState.Update))
    , rgPickMainEventMap :: !(EventMap (T m GuiState.Update))
    }

makeShownResult ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload -> Result (T m) ->
    ExprGuiM m
    ( EventMap (T m GuiState.Update)
    , WithTextPos (Widget (T m GuiState.Update))
    )
makeShownResult pl result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- rHoleResult result & transaction
        theme <- Theme.hole <$> Lens.view Theme.theme
        stdSpacing <- Spacer.getSpaceSize
        let padding = Theme.holeResultPadding theme <&> realToFrac & (* stdSpacing)
        ResultWidget.make pl (rId result) res <&> _2 %~ Element.pad padding

makeResultGroup ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    ResultsList (T m) ->
    ExprGuiM m (ResultGroup m)
makeResultGroup pl results =
    makeShownResult pl (results ^. HoleResults.rlMain)
    <&>
    \(pickMain, mainResultWidget) ->
    ResultGroup
    { rgOption =
        Menu.Option
        { Menu._oId = results ^. HoleResults.rlExtraResultsPrefixId
        , Menu._oWidget = mainResultWidget
        , Menu._oSubmenuWidgets =
            case results ^. HoleResults.rlExtra of
            [] -> Menu.SubmenuEmpty
            extras -> Menu.SubmenuItems (traverse (makeShownResult pl) extras <&> map snd)
        }
    , rgPickMainEventMap = pickMain
    }

assignCursor ::
    Monad m =>
    WidgetIds -> [Widget.Id] -> [Widget.Id] -> ExprGuiM m a -> ExprGuiM m a
assignCursor widgetIds shownMainResultsIds allShownResultIds action =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        let destId
                | Text.null searchTerm =
                      -- When selecting a result like "fac HOLE", the
                      -- cursor moves to the hidOpen of the selected
                      -- HOLE, which has a null search term. We want to
                      -- move the cursor to the search term in this case,
                      -- otherwise further actions surprisingly apply to a
                      -- random first result. (e.g: "fac (" will apply
                      -- open-paren to the first result)
                      searchTermId
                | otherwise = head (shownMainResultsIds ++ [searchTermId])

        -- Results appear and disappear when the search-string changes,
        -- but the cursor prefix signifies whether we should be on a result.
        -- When that is the case but is not currently on any of the existing results
        -- the cursor will be sent to the default one.
        shouldBeOnResult <- sub (hidResultsPrefix widgetIds)
        isOnResult <- traverse sub allShownResultIds <&> or

        action
            & if shouldBeOnResult && not isOnResult
            then Reader.local (GuiState.cursor .~ destId)
            else GuiState.assignCursor (hidOpen widgetIds) destId
    where
        searchTermId = hidOpenSearchTerm widgetIds
        sub x = GuiState.isSubCursor ?? x

makeInferredTypeAnnotation ::
    ( MonadReader env m, Theme.HasTheme env, Element.HasAnimIdPrefix env
    , MonadTransaction n0 m, Spacer.HasStdSpacing env
    ) => Sugar.Payload m0 a0 -> AnimId -> m View
makeInferredTypeAnnotation pl animId =
    Annotation.addAnnotationBackground
    <*> TypeView.make (pl ^. Sugar.plAnnotation . Sugar.aInferredType)
    <&> (^. Align.tValue)
    & Reader.local (Element.animIdPrefix .~ animId)

makeUnderCursorAssignment ::
    Monad m =>
    EventMap (T m GuiState.Update) -> [ResultsList (T m)] -> Menu.HasMoreOptions ->
    Sugar.Hole (T m) (ExpressionN m ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeUnderCursorAssignment searchTermEventMap shownResultsLists hasHiddenResults hole pl =
    do
        groupsWidgets <- traverse (makeResultGroup pl) shownResultsLists

        vspace <- Annotation.annotationSpacer
        pickFirstResult <-
            case groupsWidgets of
            [] -> ResultWidget.emptyPickEventMap
            (x:_) -> rgPickMainEventMap x & return
        let options =
                groupsWidgets <&> rgOption
                <&> Menu.optionWidgets . Lens.mapped .
                    Widget.eventMapMaker . Lens.mapped %~ (searchTermEventMap <>)
        typeView <- makeInferredTypeAnnotation pl holeAnimId
        hoverMenu <- Menu.makeHovered (vspace /-/ typeView) options hasHiddenResults
        SearchTerm.make widgetIds holeKind
            <&> Align.tValue %~ Widget.weakerEvents pickFirstResult
            <&> \searchTermWidget placement ->
                searchTermWidget <&> hoverMenu placement
    & Reader.local (Element.animIdPrefix .~ WidgetId.toAnimId (hidOpen widgetIds))
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        holeKind = hole ^. Sugar.holeKind
        holeAnimId = hidHole widgetIds & Widget.toAnimId

makeOpenSearchAreaGui ::
    Monad m =>
    EventMap (T m GuiState.Update) ->
    Sugar.Hole (T m) (ExpressionN m ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeOpenSearchAreaGui searchTermEventMap hole pl =
    do
        (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll hole widgetIds
        let shownMainResultsIds = shownResultsLists <&> rId . (^. HoleResults.rlMain)
        let allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
        makeUnderCursorAssignment searchTermEventMap shownResultsLists
            hasHiddenResults hole pl
            & assignCursor widgetIds shownMainResultsIds allShownResultIds
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
