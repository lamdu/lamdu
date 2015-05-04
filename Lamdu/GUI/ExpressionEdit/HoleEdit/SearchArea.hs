{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( makeStdWrapped
    ) where

import           Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, msum)
import           Control.MonadA (MonadA)
import           Data.List.Lens (suffixed)
import qualified Data.Map as Map
import           Data.Maybe (isJust, maybeToList, fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (traverse)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addBackground, addDarkBackground)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (EditableHoleInfo(..), HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultsList(..), Result(..), HaveHiddenResults(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as HoleResults
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea.ShownResult (PickedResult(..), ShownResult(..), pickedEventResult)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), ExpressionN)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

extraSymbol :: String
extraSymbol = "▷"

extraSymbolScaleFactor :: Fractional a => a
extraSymbolScaleFactor = 0.5

compose :: [a -> a] -> a -> a
compose = foldr (.) id

eventResultOfPickedResult :: Sugar.PickedResult -> PickedResult
eventResultOfPickedResult pr =
    PickedResult
    { _pickedEventResult =
        Widget.EventResult
        { Widget._eCursor =
            Monoid.Last $
            WidgetIds.fromEntityId <$> pr ^. Sugar.prMJumpTo
        , Widget._eAnimIdMapping =
            Monoid.Endo $ pickedResultAnimIdTranslation $ pr ^. Sugar.prIdTranslation
        }
    , _pickedIdTranslations =
        pr ^. Sugar.prIdTranslation
        & Lens.mapped . Lens.both %~ WidgetIds.fromEntityId
        & mapPrefix
    }
    where
        mapPrefix = compose . map reprefix
        reprefix (old, new) ident =
            maybe ident (WidgetId.joinId new) $ WidgetId.subId old ident
        pickedResultAnimIdTranslation idTranslations =
            -- Map only the first anim id component
            Lens.ix 0 %~ \x -> fromMaybe x $ Map.lookup x idMap
            where
                idMap =
                    idTranslations
                    & Lens.traversed . Lens.both %~
                      head . Widget.toAnimId . WidgetIds.fromEntityId
                    & Map.fromList

resultSuffix :: Lens.Prism' AnimId AnimId
resultSuffix = suffixed ["result suffix"]

afterPick ::
    Monad m => EditableHoleInfo m -> Widget.Id -> Sugar.PickedResult ->
    T m PickedResult
afterPick editableHoleInfo resultId pr =
    do
        Property.set (ehiState editableHoleInfo) HoleState.emptyState
        eventResultOfPickedResult pr
            & pickedEventResult . Widget.eCursor %~
              mappend (Monoid.Last (Just myHoleId))
            & pickedEventResult . Widget.eAnimIdMapping %~
              mappend (Monoid.Endo obliterateOtherResults)
            & return
    where
        myHoleId =
            WidgetIds.fromEntityId $ hiEntityId (ehiInfo editableHoleInfo)
        obliterateOtherResults animId =
            case animId ^? resultSuffix of
            Nothing -> animId
            Just unsuffixed
                | Lens.has (suffixed (Widget.toAnimId resultId)) unsuffixed ->
                      animId
                | otherwise -> "obliterated" : animId

makeShownResult ::
    MonadA m => EditableHoleInfo m -> Result m -> ExprGuiM m (Widget (T m), ShownResult m)
makeShownResult editableHoleInfo result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- ExprGuiM.transaction $ rHoleResult result
        config <- Config.hole <$> ExprGuiM.readConfig
        (widget, mkEventMap) <- makeHoleResultWidget (rId result) res
        let padding = Config.holeResultPadding config <&> realToFrac
        return
            ( widget & Widget.pad padding
            , ShownResult
              { srMkEventMap = mkEventMap
              , srHoleResult = res
              , srPick =
                res ^. Sugar.holeResultPick
                >>= afterPick editableHoleInfo (rId result)
              }
            )

makeExtraSymbolWidget :: MonadA m => AnimId -> Bool -> ResultsList n -> ExprGuiM m (Widget f)
makeExtraSymbolWidget animId isSelected results
    | Lens.nullOf (HoleResults.rlExtra . traverse) results = pure Widget.empty
    | otherwise =
        do
            Config.Hole{..} <- Config.hole <$> ExprGuiM.readConfig
            let
                extraSymbolColor
                    | isSelected = holeExtraSymbolColorSelected
                    | otherwise = holeExtraSymbolColorUnselected
            ExprGuiM.makeLabel extraSymbol animId
                <&> Widget.scale extraSymbolScaleFactor
                <&> Widget.tint extraSymbolColor
                >>= ExprGuiM.widgetEnv . BWidgets.hboxCenteredSpaced . (Widget.empty :) . (: [])

data ResultGroupWidgets m = ResultGroupWidgets
    { _rgwMainResult :: ShownResult m
    , _rgwMSelectedResult :: Maybe (ShownResult m) -- Can be an extra result
    , _rgwRow :: [Widget (T m)]
    }
rgwMainResult :: Lens' (ResultGroupWidgets m) (ShownResult m)
rgwMainResult f ResultGroupWidgets{..} =
    f _rgwMainResult <&> \_rgwMainResult -> ResultGroupWidgets{..}
rgwMSelectedResult :: Lens' (ResultGroupWidgets m) (Maybe (ShownResult m))
rgwMSelectedResult f ResultGroupWidgets{..} =
    f _rgwMSelectedResult <&> \_rgwMSelectedResult -> ResultGroupWidgets{..}
rgwRow :: Lens' (ResultGroupWidgets m) [Widget (T m)]
rgwRow f ResultGroupWidgets{..} =
    f _rgwRow <&> \_rgwRow -> ResultGroupWidgets{..}

makeResultGroup ::
    MonadA m =>
    EditableHoleInfo m ->
    ResultsList m ->
    ExprGuiM m (ResultGroupWidgets m)
makeResultGroup editableHoleInfo results =
    do
        Config.Hole{..} <- Config.hole <$> ExprGuiM.readConfig
        (mainResultWidget, shownMainResult) <-
            makeShownResult editableHoleInfo mainResult
        let mainResultHeight = mainResultWidget ^. Widget.height
            makeExtra =
                results ^. HoleResults.rlExtra
                & makeExtraResultsWidget editableHoleInfo mainResultHeight
        (mSelectedResult, extraResWidget) <-
            if mainResultWidget ^. Widget.isFocused
            then do
                widget <- snd <$> makeExtra
                return (Just shownMainResult, widget)
            else do
                cursorOnExtra <-
                    results ^. HoleResults.rlExtraResultsPrefixId
                    & WE.isSubCursor & ExprGuiM.widgetEnv
                if cursorOnExtra
                    then makeExtra
                    else
                    Widget.empty
                    & focusFirstExtraResult (results ^. HoleResults.rlExtra)
                    <&> (,) Nothing
        let isSelected = Lens.has Lens._Just mSelectedResult
        extraSymbolWidget <-
            makeExtraSymbolWidget (Widget.toAnimId (rId mainResult)) isSelected
            results
        return ResultGroupWidgets
            { _rgwMainResult = shownMainResult
            , _rgwMSelectedResult = mSelectedResult
            , _rgwRow = [mainResultWidget, extraSymbolWidget, extraResWidget]
            }
    where
        mainResult = results ^. HoleResults.rlMain
        focusFirstExtraResult [] = return
        focusFirstExtraResult (result:_) = makeFocusable (rId result)

makeExtraResultsWidget ::
    MonadA m => EditableHoleInfo m -> Anim.R -> [Result m] ->
    ExprGuiM m (Maybe (ShownResult m), Widget (T m))
makeExtraResultsWidget _ _ [] = return (Nothing, Widget.empty)
makeExtraResultsWidget editableHoleInfo mainResultHeight extraResults@(firstResult:_) =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
            mkResWidget result =
                do
                    isOnResult <-
                        WE.isSubCursor (rId result)
                        & ExprGuiM.widgetEnv
                    (widget, shownResult) <- makeShownResult editableHoleInfo result
                    return
                        ( shownResult <$ guard isOnResult
                        , widget
                        )
        (mResults, widgets) <-
            unzip <$> traverse mkResWidget extraResults
        let headHeight = head widgets ^. Widget.height
            height = min mainResultHeight headHeight
        return
            ( msum mResults
            , Box.vboxAlign 0 widgets
              & addBackground (Widget.toAnimId (rId firstResult))
                (Config.layers config) holeOpenBGColor
              & Widget.size .~ Vector2 0 height
              & Widget.translate (Vector2 0 (0.5 * (height - headHeight)))
            )

makeFocusable ::
    (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId

makeHoleResultWidget ::
    MonadA m => Widget.Id ->
    Sugar.HoleResult (Name m) m ->
    ExprGuiM m (Widget (T m), ExprGuiM m (Widget.EventHandlers (T m)))
makeHoleResultWidget resultId holeResult =
    do
        Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole
        let mkEventMap =
              do
                  -- Create a hidden result widget that we never display, but only
                  -- keep the event map from. We always tell it that it has focus,
                  -- so that even if we're on the search term, we can have valid
                  -- event maps of any result (we actually use the first one's
                  -- event map)
                  hiddenResultWidget <-
                    mkWidget
                    & ExprGuiM.localEnv (WE.envCursor .~ idWithinResultWidget)
                  hiddenResultWidget ^. Widget.eventMap & return
        widget <-
            mkWidget
            <&> Widget.animFrame %~ Anim.mapIdentities (<> (resultSuffix # Widget.toAnimId resultId))
            <&> Widget.eventMap .~ mempty
            >>= makeFocusable resultId
            >>= ExprGuiM.widgetEnv . BWidgets.liftLayerInterval
        return (widget, mkEventMap)
    where
        mkWidget =
            holeResultConverted
            & postProcessSugar
            & ExprGuiM.makeSubexpression 0
            <&> (^. ExpressionGui.egWidget)
        holeResultEntityId =
            holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
        idWithinResultWidget =
            holeResult ^. Sugar.holeResultHoleTarget
            & fromMaybe holeResultEntityId
            & WidgetIds.fromEntityId
        holeResultConverted = holeResult ^. Sugar.holeResultConverted

postProcessSugar ::
    MonadA m =>
    ExpressionN m Sugar.IsInjected ->
    ExpressionN m ExprGuiM.Payload
postProcessSugar expr =
    expr
    <&> toPayload
    & SugarLens.holeArgs . Sugar.plData . ExprGuiM.plShowType .~ ExprGuiM.ShowType

toPayload :: Sugar.IsInjected -> ExprGuiM.Payload
toPayload isInjected =
    ExprGuiM.emptyPayload NearestHoles.none
    & ExprGuiM.plShowType .~ ExprGuiM.DoNotShowType
    & ExprGuiM.plInjected .~
      case isInjected of
      Sugar.NotInjected -> []
      Sugar.Injected -> [True]

makeNoResults :: MonadA m => AnimId -> ExprGuiM m (Widget (T m))
makeNoResults animId =
    ExpressionGui.makeLabel "(No results)" animId
    <&> (^. ExpressionGui.egWidget)

makeHiddenResultsMWidget :: MonadA m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe (Widget f))
makeHiddenResultsMWidget HaveHiddenResults myId =
    Just <$> ExprGuiM.makeLabel "..." (Widget.toAnimId myId)
makeHiddenResultsMWidget NoHiddenResults _ = return Nothing

addMResultPicker :: MonadA m => Maybe (ShownResult m) -> ExprGuiM m ()
addMResultPicker mSelectedResult =
    case mSelectedResult of
    Nothing -> return ()
    Just res -> ExprGuiM.addResultPicker $ (^. pickedEventResult) <$> srPick res

layoutResults ::
    MonadA m =>
    [[Widget (T m)]] -> HaveHiddenResults -> Widget.Id -> ExprGuiM m (Widget (T m))
layoutResults rows hiddenResults myId
    | null rows = makeNoResults (Widget.toAnimId myId)
    | otherwise =
        do
            hiddenResultsWidgets <- maybeToList <$> makeHiddenResultsMWidget hiddenResults myId
            let grid =
                  rows
                  & Lens.mapped . Lens.mapped %~ (,) (Vector2 0 0.5)
                  & Grid.make & Grid.toWidget
                  & EventMap.blockDownEvents
            grid : hiddenResultsWidgets & Box.vboxCentered & return

makeResultsWidget ::
    MonadA m => EditableHoleInfo m ->
    [ResultsList m] -> HaveHiddenResults ->
    ExprGuiM m (Maybe (ShownResult m), Widget (T m))
makeResultsWidget editableHoleInfo shownResultsLists hiddenResults =
    do
        groupsWidgets <- traverse (makeResultGroup editableHoleInfo) shownResultsLists
        let
            mSelectedResult = groupsWidgets ^? Lens.traversed . rgwMSelectedResult . Lens._Just
            mFirstResult = groupsWidgets ^? Lens.traversed . rgwMainResult
            mResult = mSelectedResult <|> mFirstResult
            rows = groupsWidgets ^.. Lens.traversed . rgwRow
        addMResultPicker mResult
        widget <- layoutResults rows hiddenResults myId
        return (mResult, widget)
    where
        myId = editableHoleInfo & ehiInfo & hiIds & hidOpen

assignHoleEditCursor ::
    MonadA m =>
    EditableHoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
    ExprGuiM m a ->
    ExprGuiM m a
assignHoleEditCursor editableHoleInfo shownMainResultsIds allShownResultIds searchTermId action =
    do
        cursor <- ExprGuiM.widgetEnv WE.readCursor
        let
            sub = isJust . flip Widget.subId cursor
            holeInfo = ehiInfo editableHoleInfo
            WidgetIds{..} = hiIds holeInfo
            shouldBeOnResult = sub hidResultsPrefix
            isOnResult = any sub allShownResultIds
            -- TODO: Instead of assignSource, use setCursor
            -- vs. assignCursor?
            assignSource
                | shouldBeOnResult && not isOnResult = cursor
                | otherwise = hidOpen
            destId
                | null (HoleInfo.ehiSearchTerm editableHoleInfo) = searchTermId
                | otherwise = head (shownMainResultsIds ++ [searchTermId])
        ExprGuiM.assignCursor assignSource destId action

makeUnderCursorAssignment ::
    MonadA m =>
    [ResultsList m] -> HaveHiddenResults ->
    EditableHoleInfo m -> ExprGuiM m (ExpressionGui m)
makeUnderCursorAssignment shownResultsLists hasHiddenResults editableHoleInfo =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config

        (mShownResult, resultsWidget) <-
            makeResultsWidget editableHoleInfo shownResultsLists hasHiddenResults

        (searchTermEventMap, resultsEventMap) <-
            EventMap.makeOpenEventMaps editableHoleInfo mShownResult

        -- We make our own type view here instead of
        -- ExpressionGui.stdWrap, because we want to synchronize the
        -- active BG width with the inferred type width
        typeView <-
            ExpressionGui.makeTypeView (resultsWidget ^. Widget.width)
            (hiEntityId holeInfo) (hiInferredType holeInfo)

        vspace <- ExpressionGui.inferredSpacer
        hoverResultsWidget <-
            resultsWidget
            & Widget.width %~ max (typeView ^. Widget.width)
            & Widget.strongerEvents resultsEventMap .
              addBackground (Widget.toAnimId hidResultsPrefix) (Config.layers config)
              holeOpenBGColor
            & ExpressionGui.fromValueWidget
            & Layout.addAfter Layout.Vertical [(0.5, vspace), (0.5, typeView)]
            & addDarkBackground (Widget.toAnimId hidResultsPrefix)
            <&> (^. ExpressionGui.egWidget)
            >>= ExprGuiM.widgetEnv . BWidgets.liftLayerInterval
        searchTermGui <- SearchTerm.make holeInfo (Just editableHoleInfo)
        searchTermGui
            & ExpressionGui.egWidget %~ Widget.weakerEvents searchTermEventMap
            & alignment .~ 0
            & Layout.addAfter Layout.Vertical
              [(0, hoverResultsWidget)]
            & alignment .~ searchTermGui ^. alignment
            & return
    where
        alignment :: Lens' (Layout f) Box.Alignment
        alignment = Layout.absAlignedWidget . _1
        WidgetIds{..} = hiIds holeInfo
        holeInfo = ehiInfo editableHoleInfo

makeOpenSearchTermGui ::
    MonadA m =>
    Sugar.Payload m ExprGuiM.Payload ->
    EditableHoleInfo m ->
    ExprGuiM m (ExpressionGui m)
makeOpenSearchTermGui pl editableHoleInfo =
    do
        (shownResultsLists, hasHiddenResults) <-
            -- Don't generate results of open holes inside hole results
            if isHoleResult
            then return ([], HaveHiddenResults)
            else HoleResults.makeAll editableHoleInfo
        let
            shownMainResultsIds =
                rId . (^. HoleResults.rlMain) <$> shownResultsLists
            allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
        makeUnderCursorAssignment shownResultsLists
            hasHiddenResults editableHoleInfo
            & assignHoleEditCursor editableHoleInfo shownMainResultsIds
              allShownResultIds (holeInfo & hiIds & hidOpenSearchTerm)
            & ExpressionGui.wrapExprEventMap pl
    where
        isHoleResult =
            Lens.nullOf
            (Sugar.plData . ExprGuiM.plStoredEntityIds . Lens.traversed) pl
        holeInfo = ehiInfo editableHoleInfo

fdConfig :: Config.Hole -> FocusDelegator.Config
fdConfig Config.Hole{..} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = holeOpenKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Hole", "Open"]
    , FocusDelegator.focusParentKeys = holeCloseKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Hole", "Close"]
    }

-- Has an ExpressionGui.stdWrap/typeView under the search term
makeStdWrapped ::
    MonadA m =>
    Sugar.Payload m ExprGuiM.Payload ->
    HoleInfo m -> Maybe (EditableHoleInfo m) ->
    ExprGuiM m (ExpressionGui m)
makeStdWrapped pl holeInfo mEditableHoleInfo =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
            WidgetIds{..} = hiIds holeInfo
            fdWrap =
                ExpressionGui.egWidget %%~
                ExprGuiM.widgetEnv .
                BWidgets.makeFocusDelegator (fdConfig (Config.hole config))
                FocusDelegator.FocusEntryChild hidClosedSearchArea
        closedSearchTermGui <-
            SearchTerm.make holeInfo mEditableHoleInfo
            >>= fdWrap & ExpressionGui.stdWrap pl
        isSelected <- ExprGuiM.widgetEnv $ WE.isSubCursor hidOpen
        case mEditableHoleInfo of
            Just editableHoleInfo
                | isSelected ->
                      makeOpenSearchTermGui pl editableHoleInfo
                      -- ideally the fdWrap would be "inside" the
                      -- type-view addition and stdWrap, but it's not
                      -- important in the case the FD is selected, and
                      -- it is harder to implement, so just wrap it
                      -- here
                      >>= fdWrap
                      <&> (`Layout.hoverInPlaceOf` closedSearchTermGui)
            _ -> return closedSearchTermGui
