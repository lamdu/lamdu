{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchTermGui
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, msum)
import           Control.MonadA (MonadA)
import           Data.List.Lens (suffixed)
import qualified Data.Map as Map
import           Data.Maybe (isJust, fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
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
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ShownResult (PickedResult(..), ShownResult(..), pickedEventResult, pickedIdTranslations)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), ExpressionN)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

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
        { Widget._eCursor = Monoid.Last Nothing
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
    Monad m =>
    EditableHoleInfo m -> Widget.Id -> Maybe Sugar.EntityId ->
    Sugar.PickedResult -> T m PickedResult
afterPick editableHoleInfo resultId mFirstHoleInside pr =
    do
        Property.set (ehiState editableHoleInfo) HoleState.emptyState
        result
            & pickedEventResult . Widget.eCursor .~ Monoid.Last (Just cursorId)
            & pickedEventResult . Widget.eAnimIdMapping %~
              mappend (Monoid.Endo obliterateOtherResults)
            & return
    where
        result = eventResultOfPickedResult pr
        cursorId =
            mFirstHoleInside <&> WidgetIds.fromEntityId
            & fromMaybe myHoleId
            & result ^. pickedIdTranslations
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
        let mFirstHoleInside =
                res ^? Sugar.holeResultConverted
                . SugarLens.holePayloads . Sugar.plEntityId
        return
            ( widget & Widget.pad padding
            , ShownResult
              { srMkEventMap = mkEventMap
              , srHoleResult = res
              , srPick =
                res ^. Sugar.holeResultPick
                >>= afterPick editableHoleInfo (rId result) mFirstHoleInside
              }
            )

makeExtraSymbolWidget :: MonadA m => AnimId -> Bool -> ResultsList n -> ExprGuiM m (Widget f)
makeExtraSymbolWidget animId isSelected results
    | Lens.nullOf (HoleResults.rlExtra . traverse) results = pure Widget.empty
    | otherwise =
        do
            Config.Hole{..} <- Config.hole <$> ExprGuiM.readConfig
            let extraSymbolColor
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
    , _rgwPadding :: Widget.R
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
rgwPadding :: Lens' (ResultGroupWidgets m) Widget.R
rgwPadding f ResultGroupWidgets{..} =
    f _rgwPadding <&> \_rgwPadding -> ResultGroupWidgets{..}

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
        let makeExtra =
                results ^. HoleResults.rlExtra
                & makeExtraResultsWidget editableHoleInfo mainResultHeight
        (mSelectedResult, extraResWidget, extraPadding) <-
            if mainResultWidget ^. Widget.isFocused
            then do
                (_, extraResWidget, extraPadding) <- makeExtra
                return (Just shownMainResult, extraResWidget, extraPadding)
            else do
                cursorOnExtra <-
                    results ^. HoleResults.rlExtraResultsPrefixId
                    & WE.isSubCursor & ExprGuiM.widgetEnv
                if cursorOnExtra
                    then makeExtra
                    else
                    Widget.empty
                    & focusFirstExtraResult (results ^. HoleResults.rlExtra)
                    <&> \x -> (Nothing, x, 0)
        let isSelected = Lens.has Lens._Just mSelectedResult
        extraSymbolWidget <-
            makeExtraSymbolWidget (Widget.toAnimId (rId mainResult)) isSelected
            results
        return ResultGroupWidgets
            { _rgwMainResult = shownMainResult
            , _rgwMSelectedResult = mSelectedResult
            , _rgwRow = [mainResultWidget, extraSymbolWidget, extraResWidget]
            , _rgwPadding = extraPadding
            }
    where
        mainResult = results ^. HoleResults.rlMain
        focusFirstExtraResult [] = return
        focusFirstExtraResult (result:_) = makeFocusable (rId result)

makeExtraResultsWidget ::
    MonadA m => EditableHoleInfo m -> Anim.R -> [Result m] ->
    ExprGuiM m (Maybe (ShownResult m), Widget (T m), Widget.R)
makeExtraResultsWidget _ _ [] = return (Nothing, Widget.empty, 0)
makeExtraResultsWidget editableHoleInfo mainResultHeight extraResults@(firstResult:_) =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        let mkResWidget result =
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
        let height = min mainResultHeight headHeight
        let widget =
                Box.vboxAlign 0 widgets
                & addBackground (Widget.toAnimId (rId firstResult))
                    (Config.layers config) holeOpenBGColor
        return
            ( msum mResults
            , widget
                & Widget.size .~ Vector2 0 height
                & Widget.translate (Vector2 0 (0.5 * (height - headHeight)))
            , (widget ^. Widget.size . _2) - 0.5 * (headHeight + mainResultHeight)
                & max 0
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
                -- Create a hidden result widget that we never display, but only
                -- keep the event map from. We always tell it that it has focus,
                -- so that even if we're on the search term, we can have valid
                -- event maps of any result (we actually use the first one's
                -- event map)
                ExprGuiM.localEnv (WE.envCursor .~ idWithinResultWidget) mkWidget
                <&> (^. Widget.eventMap)
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
            & ExprGuiM.makeSubexpression (const 0)
            <&> (^. ExpressionGui.egWidget)
        holeResultEntityId =
            holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
        idWithinResultWidget =
            holeResult
            ^? Sugar.holeResultConverted
            . SugarLens.holePayloads . Sugar.plEntityId
            & fromMaybe holeResultEntityId
            & WidgetIds.fromEntityId
        holeResultConverted = holeResult ^. Sugar.holeResultConverted

postProcessSugar ::
    MonadA m =>
    ExpressionN m Sugar.IsInjected ->
    ExpressionN m ExprGuiT.Payload
postProcessSugar expr =
    expr
    <&> toPayload
    & SugarLens.holeArgs . Sugar.plData . ExprGuiT.plShowAnnotation
    .~ ExprGuiT.alwaysShowAnnotations

toPayload :: Sugar.IsInjected -> ExprGuiT.Payload
toPayload isInjected =
    ExprGuiT.emptyPayload NearestHoles.none
    & ExprGuiT.plShowAnnotation .~ ExprGuiT.neverShowAnnotations
    & ExprGuiT.plInjected .~
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

calcPadding :: [ResultGroupWidgets m] -> Widget.R
calcPadding =
    foldl step 0
    where
        step accum item =
            max 0 (accum - (head (item ^. rgwRow) ^. Widget.size . _2))
            + (item ^. rgwPadding)

layoutResults ::
    MonadA m =>
    [ResultGroupWidgets m] -> HaveHiddenResults -> Widget.Id ->
    ExprGuiM m (Widget (T m))
layoutResults groups hiddenResults myId
    | null groups = makeNoResults (Widget.toAnimId myId)
    | otherwise =
        do
            hiddenResultsWidgets <-
                makeHiddenResultsMWidget hiddenResults myId <&> (^.. Lens._Just)
            let grid =
                  rows
                  & Lens.mapped . Lens.mapped %~ (,) (Vector2 0 0.5)
                  & Grid.make & Grid.toWidget
                  & EventMap.blockDownEvents
            let padHeight =
                    calcPadding groups
                    - sum (hiddenResultsWidgets ^.. Lens.traversed . Widget.size . _2)
                    & max 0
            grid : hiddenResultsWidgets & Box.vboxCentered
                & Widget.assymetricPad 0 (Vector2 0 padHeight)
                & return
    where
        rows = groups ^.. Lens.traversed . rgwRow

makeResultsWidget ::
    MonadA m => EditableHoleInfo m ->
    [ResultsList m] -> HaveHiddenResults ->
    ExprGuiM m (Maybe (ShownResult m), Widget (T m))
makeResultsWidget editableHoleInfo shownResultsLists hiddenResults =
    do
        groupsWidgets <- traverse (makeResultGroup editableHoleInfo) shownResultsLists
        let mSelectedResult = groupsWidgets ^? Lens.traversed . rgwMSelectedResult . Lens._Just
        let mFirstResult = groupsWidgets ^? Lens.traversed . rgwMainResult
        let mResult = mSelectedResult <|> mFirstResult
        addMResultPicker mResult
        widget <- layoutResults groupsWidgets hiddenResults myId
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
        let sub = isJust . flip Widget.subId cursor
        let shouldBeOnResult = sub hidResultsPrefix
        let isOnResult = any sub allShownResultIds
        -- TODO: Instead of assignSource, use setCursor
        -- vs. assignCursor?
        let assignSource
                | shouldBeOnResult && not isOnResult = cursor
                | otherwise = hidOpen
        ExprGuiM.assignCursor assignSource destId action
    where
        WidgetIds{..} = ehiInfo editableHoleInfo & hiIds
        destId
            | null (HoleInfo.ehiSearchTerm editableHoleInfo) = searchTermId
            | otherwise = head (shownMainResultsIds ++ [searchTermId])

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
            ExpressionGui.makeTypeView (hiInferredType holeInfo)
            (Widget.toAnimId hidHole)

        vspace <- ExpressionGui.annotationSpacer
        hoverResultsWidget <-
            resultsWidget
            & Widget.width %~ max (typeView ^. ExpressionGui.egWidget . Widget.width)
            & Widget.strongerEvents resultsEventMap .
              addBackground (Widget.toAnimId hidResultsPrefix) (Config.layers config)
              holeOpenBGColor
            & ExpressionGui.fromValueWidget
            & Layout.addAfter Layout.Vertical [vspace, typeView]
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
    Sugar.Payload m ExprGuiT.Payload ->
    EditableHoleInfo m ->
    ExprGuiM m (ExpressionGui m)
makeOpenSearchTermGui pl editableHoleInfo =
    do
        (shownResultsLists, hasHiddenResults) <-
            -- Don't generate results of open holes inside hole results
            if isHoleResult
            then return ([], HaveHiddenResults)
            else HoleResults.makeAll editableHoleInfo
        let shownMainResultsIds =
                rId . (^. HoleResults.rlMain) <$> shownResultsLists
        let allShownResultIds =
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
            (Sugar.plData . ExprGuiT.plStoredEntityIds . Lens.traversed) pl
        holeInfo = ehiInfo editableHoleInfo
