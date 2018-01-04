{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
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
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultGroup(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as ResultGroups
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

data ResultOption m = ResultOption
    { roOption :: !(Menu.Option (ExprGuiM m) (T m GuiState.Update))
    , roPickMainEventMap :: !(EventMap (T m GuiState.Update))
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

makeResultOption ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    ResultGroup (T m) ->
    ExprGuiM m (ResultOption m)
makeResultOption pl results =
    makeShownResult pl (results ^. ResultGroups.rgMain)
    <&>
    \(pickMain, mainResultWidget) ->
    ResultOption
    { roOption =
        Menu.Option
        { Menu._oId = results ^. ResultGroups.rgExtraResultsPrefixId
        , Menu._oWidget = mainResultWidget
        , Menu._oSubmenuWidgets =
            case results ^. ResultGroups.rgExtra of
            [] -> Menu.SubmenuEmpty
            extras -> Menu.SubmenuItems (traverse (makeShownResult pl) extras <&> map snd)
        }
    , roPickMainEventMap = pickMain
    }

assignCursor :: Monad m => WidgetIds -> [Widget.Id] -> ExprGuiM m a -> ExprGuiM m a
assignCursor widgetIds resultIds action =
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
                | otherwise = head (resultIds ++ [searchTermId])

        -- Results appear and disappear when the search-string changes,
        -- but the cursor prefix signifies whether we should be on a result.
        -- When that is the case but is not currently on any of the existing results
        -- the cursor will be sent to the default one.
        shouldBeOnResult <- sub (hidResultsPrefix widgetIds)
        isOnResult <- traverse sub resultIds <&> or

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

emptyPickEventMap ::
    (MonadReader env m, HasConfig env, Applicative f) =>
    m (EventMap (f GuiState.Update))
emptyPickEventMap =
    Lens.view Config.config <&> Config.hole <&> keys <&> mkEventMap
    where
        keys c = Config.holePickResultKeys c ++ Config.holePickAndMoveToNextHoleKeys c
        mkEventMap k =
            E.keysEventMap k (E.Doc ["Edit", "Result", "Pick (N/A)"]) (pure ())

makeUnderCursorAssignment ::
    Monad m =>
    EventMap (T m GuiState.Update) -> Menu.HasMoreOptions ->
    (Text -> Bool) -> Sugar.Payload (T m) ExprGui.Payload -> [ResultOption m] ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeUnderCursorAssignment searchTermEventMap hasHiddenResults allowedTerms pl groupsWidgets =
    do
        vspace <- Annotation.annotationSpacer
        pickFirstResult <-
            case groupsWidgets of
            [] -> emptyPickEventMap
            (x:_) -> roPickMainEventMap x & return
        let options =
                groupsWidgets <&> roOption
                <&> Menu.optionWidgets . Lens.mapped .
                    Widget.eventMapMaker . Lens.mapped %~ (searchTermEventMap <>)
        typeView <- makeInferredTypeAnnotation pl holeAnimId
        hoverMenu <- Menu.makeHovered (vspace /-/ typeView) options hasHiddenResults
        SearchTerm.make widgetIds allowedTerms
            <&> Align.tValue %~ Widget.weakerEvents pickFirstResult
            <&> \searchTermWidget placement ->
                searchTermWidget <&> hoverMenu placement
    & Reader.local (Element.animIdPrefix .~ WidgetId.toAnimId (hidOpen widgetIds))
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        holeAnimId = hidHole widgetIds & Widget.toAnimId

makeOpenSearchAreaGui ::
    Monad m =>
    EventMap (T m GuiState.Update) ->
    T m [Sugar.HoleOption (T m) (ExpressionN m ())] ->
    Maybe (Sugar.OptionLiteral (T m) (ExpressionN m ())) ->
    (Text -> Bool) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeOpenSearchAreaGui searchTermEventMap options mOptionLiteral allowedTerms pl =
    do
        (shownResultGroups, hasHiddenResults) <- ResultGroups.makeAll options mOptionLiteral widgetIds
        traverse (makeResultOption pl) shownResultGroups
            >>= makeUnderCursorAssignment searchTermEventMap hasHiddenResults allowedTerms pl
            & assignCursor widgetIds (shownResultGroups <&> rId . (^. ResultGroups.rgMain))
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
