{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings #-}
-- | The search area (search term + results) of an open/active hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchAreaGui
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultsList(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as HoleResults
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget (makeHoleResultWidget)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
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
    Sugar.Payload f ExprGui.Payload -> Result m ->
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
        makeHoleResultWidget pl (rId result) res <&> _2 %~ Element.pad padding

makeResultGroup ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    ResultsList m ->
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

emptyPickEventMap ::
    (Monad m, Applicative f) => ExprGuiM m (EventMap (f GuiState.Update))
emptyPickEventMap =
    Lens.view Config.config <&> Config.hole <&> keys <&> mkEventMap
    where
        keys c = Config.holePickResultKeys c ++ Config.holePickAndMoveToNextHoleKeys c
        mkEventMap k =
            E.keysEventMap k (E.Doc ["Edit", "Result", "Pick (N/A)"]) (pure ())

assignHoleEditCursor ::
    Monad m =>
    WidgetIds -> Text -> [Widget.Id] -> [Widget.Id] ->
    ExprGuiM m a ->
    ExprGuiM m a
assignHoleEditCursor widgetIds searchTerm shownMainResultsIds allShownResultIds action =
    do
        shouldBeOnResult <- sub (hidResultsPrefix widgetIds)
        isOnResult <- traverse sub allShownResultIds <&> or
        let assignSource
                | shouldBeOnResult && not isOnResult =
                    Reader.local (GuiState.cursor .~ destId)
                | otherwise =
                    GuiState.assignCursor (hidOpen widgetIds) destId
        assignSource action
    where
        searchTermId = hidOpenSearchTerm widgetIds
        sub x = GuiState.isSubCursor ?? x
        destId
            | Text.null searchTerm =
                  -- When selecting a result like "fac HOLE", the
                  -- cursor moves sto the hidOpen of the selected
                  -- HOLE, which has a null search term. We want to
                  -- move the cursor to the search term in this case,
                  -- otherwise further actions surprisingly apply to a
                  -- random first result. (e.g: "fac (" will apply
                  -- open-paren to the first result)
                  searchTermId
            | otherwise = head (shownMainResultsIds ++ [searchTermId])

makeUnderCursorAssignment ::
    Monad m =>
    EventMap (T m GuiState.Update) -> [ResultsList m] -> Menu.HasMoreOptions ->
    Sugar.Hole (T m) (ExpressionN m ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    WidgetIds ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeUnderCursorAssignment searchTermEventMap shownResultsLists hasHiddenResults hole pl widgetIds =
    do
        -- We make our own type view here instead of stdWrap, because
        -- we want to synchronize the active BG width with the
        -- inferred type width
        typeView <-
            Annotation.addAnnotationBackground
            <*> TypeView.make (pl ^. Sugar.plAnnotation . Sugar.aInferredType)
            <&> (^. Align.tValue)
            & Reader.local (Element.animIdPrefix .~ holeAnimId)

        groupsWidgets <- traverse (makeResultGroup pl) shownResultsLists

        vspace <- Annotation.annotationSpacer
        literalEventMap <- EventMap.makeLiteralEventMap holeKind widgetIds
        pickFirstResult <-
            case groupsWidgets of
            [] -> emptyPickEventMap
            (x:_) -> rgPickMainEventMap x & return
        searchTermWidget <-
            SearchTerm.make widgetIds holeKind
            <&> Align.tValue %~ Hover.anchor . E.weakerEvents (pickFirstResult <> literalEventMap)
        mkHoverOptions <- Menu.hoverOptions
        let options =
                groupsWidgets <&> rgOption
                <&> Menu.optionWidgets . Lens.mapped %~ E.strongerEvents searchTermEventMap
        resultsMenu <- Menu.make (typeView ^. Element.width) options hasHiddenResults
        return $
            \placement ->
            searchTermWidget
            & Align.tValue %~
                Hover.hoverInPlaceOf
                (mkHoverOptions placement (vspace /-/ typeView) resultsMenu
                    (searchTermWidget ^. Align.tValue))
    & Reader.local (Element.animIdPrefix .~ WidgetId.toAnimId (hidOpen widgetIds))
    where
        holeKind = hole ^. Sugar.holeKind
        holeAnimId = hidHole widgetIds & Widget.toAnimId

makeOpenSearchAreaGui ::
    Monad m =>
    EventMap (T m GuiState.Update) ->
    Sugar.Hole (T m) (ExpressionN m ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    WidgetIds ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeOpenSearchAreaGui searchTermEventMap hole pl widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll hole searchTerm widgetIds
        let shownMainResultsIds = shownResultsLists <&> rId . (^. HoleResults.rlMain)
        let allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
        makeUnderCursorAssignment searchTermEventMap shownResultsLists
            hasHiddenResults hole pl widgetIds
            & assignHoleEditCursor widgetIds searchTerm shownMainResultsIds allShownResultIds
