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
import           GUI.Momentu (Widget, Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
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
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui (ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data ResultGroup m = ResultGroup
    { rgOption :: !(Menu.Option (ExprGuiM m) (T m GuiState.Update))
    , rgPickMainEventMap :: !(Widget.EventMap (T m GuiState.Update))
    }

makeShownResult ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload -> Result m ->
    ExprGuiM m
    ( Widget.EventMap (T m GuiState.Update)
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
    (Monad m, Applicative f) => ExprGuiM m (Widget.EventMap (f GuiState.Update))
emptyPickEventMap =
    Lens.view Config.config <&> Config.hole <&> keys <&> mkEventMap
    where
        keys c = Config.holePickResultKeys c ++ Config.holePickAndMoveToNextHoleKeys c
        mkEventMap k =
            Widget.keysEventMap k (E.Doc ["Edit", "Result", "Pick (N/A)"]) (pure ())

makeResultsWidget ::
    Monad m =>
    Widget.R -> Sugar.Payload f ExprGui.Payload ->
    [ResultsList m] -> Menu.HasMoreOptions ->
    ExprGuiM m (Widget.EventMap (T m GuiState.Update), Hover.Ordered (Widget (T m GuiState.Update)))
makeResultsWidget minWidth pl shownResultsLists hiddenResults =
    do
        groupsWidgets <- traverse (makeResultGroup pl) shownResultsLists
        pickResultEventMap <-
            case groupsWidgets of
            [] -> emptyPickEventMap
            (x:_) -> rgPickMainEventMap x & return
        Menu.layout minWidth (groupsWidgets <&> rgOption) hiddenResults
            <&> (,) pickResultEventMap

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

resultsHoverOptions ::
    ( MonadReader env m, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , Functor f
    ) =>
    m
    (Menu.Placement ->
     (Widget (f GuiState.Update) -> Widget (f GuiState.Update)) ->
     Hover.Ordered (Widget (f GuiState.Update)) ->
     Hover.AnchoredWidget (f GuiState.Update) ->
     [Hover.AnchoredWidget (f GuiState.Update)])
resultsHoverOptions =
    Hover.hover <&> \hover pos addAnnotation results searchTerm ->
    let resultsAbove alignment =
            results ^. Hover.backward & hover & Aligned alignment
        annotatedTerm alignment =
            searchTerm & Widget.widget %~ addAnnotation & Aligned alignment
        aboveRight = resultsAbove 0 /-/ annotatedTerm 0
        aboveLeft =
            resultsAbove 1
            /-/ annotatedTerm 1
        annotatedResultsBelow = results ^. Hover.forward & addAnnotation & hover
        resultsBelow = results ^. Hover.forward & hover
        belowRight =
            Aligned 0 searchTerm
            /-/
            Aligned 0 annotatedResultsBelow
        belowLeft =
            Aligned 1 searchTerm
            /-/
            Aligned 1 annotatedResultsBelow
        centerRight = annotatedTerm 0.5 /|/ Aligned 0.5 resultsBelow
        rightAbove = annotatedTerm 1 /|/ resultsAbove 1
        leftAbove = resultsAbove 1 /|/ annotatedTerm 1
    in  case pos of
        Menu.Above ->
            [ aboveRight
            , aboveLeft
            ]
        Menu.AnyPlace ->
            [ belowRight
            , aboveRight
            , belowLeft
            , aboveLeft
            , centerRight
            ]
        Menu.Below ->
            [ belowRight
            , belowLeft
            , rightAbove
            , leftAbove
            ]
        <&> (^. Align.value)

makeUnderCursorAssignment ::
    Monad m =>
    [ResultsList m] -> Menu.HasMoreOptions ->
    Sugar.Hole (T m) (ExpressionN m ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    WidgetIds ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeUnderCursorAssignment shownResultsLists hasHiddenResults hole pl widgetIds =
    do
        -- We make our own type view here instead of stdWrap, because
        -- we want to synchronize the active BG width with the
        -- inferred type width
        typeView <-
            Annotation.addAnnotationBackground
            <*> TypeView.make (pl ^. Sugar.plAnnotation . Sugar.aInferredType)
            <&> (^. Align.tValue)
            & Reader.local (Element.animIdPrefix .~ holeAnimId)

        searchTermEventMap <- EventMap.makeSearchTermEditEventMap holeKind widgetIds

        (pickFirstResult, resultsWidgets) <-
            makeResultsWidget (typeView ^. Element.width) pl
            shownResultsLists hasHiddenResults
            <&> _2 . Lens.mapped %~ E.strongerEvents searchTermEventMap

        vspace <- Annotation.annotationSpacer
        let addAnnotation x = x /-/ vspace /-/ typeView
        literalEventMap <- EventMap.makeLiteralEventMap holeKind widgetIds
        searchTermWidget <-
            SearchTerm.make widgetIds holeKind
            <&> Align.tValue %~ Hover.anchor . E.weakerEvents (pickFirstResult <> literalEventMap)
        mkOptions <-
            resultsHoverOptions
            & Reader.local (Element.animIdPrefix .~ WidgetId.toAnimId (hidOpen widgetIds))
        return $
            \placement ->
            searchTermWidget
            & Align.tValue %~
                Hover.hoverInPlaceOf
                (mkOptions placement addAnnotation resultsWidgets
                    (searchTermWidget ^. Align.tValue))
    where
        holeKind = hole ^. Sugar.holeKind
        holeAnimId = hidHole widgetIds & Widget.toAnimId

makeOpenSearchAreaGui ::
    Monad m =>
    Sugar.Hole (T m) (ExpressionN m ()) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    WidgetIds ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m GuiState.Update)))
makeOpenSearchAreaGui hole pl widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll hole searchTerm widgetIds
        let shownMainResultsIds = shownResultsLists <&> rId . (^. HoleResults.rlMain)
        let allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
        makeUnderCursorAssignment shownResultsLists
            hasHiddenResults hole pl widgetIds
            & assignHoleEditCursor widgetIds searchTerm shownMainResultsIds allShownResultIds
