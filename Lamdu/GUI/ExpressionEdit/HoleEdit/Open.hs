{-# LANGUAGE TemplateHaskell, PatternGuards, NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TypeFamilies #-}
-- | The search area (search term + results) of an open/active hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchAreaGui
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Monad (msum)
import           Data.List.Lens (suffixed)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.CharClassification (operatorChars)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addBackground, addDarkBackground)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultsList(..), Result(..), HaveHiddenResults(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as HoleResults
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ShownResult (PickedResult(..), ShownResult(..), pickedEventResult, pickedIdTranslations)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..), ExpressionN)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data ResultGroupWidgets m = ResultGroupWidgets
    { _rgwMainResult :: ShownResult m
    , _rgwMSelectedResult :: Maybe (ShownResult m) -- Can be an extra result
    , _rgwRow :: [Widget (T m Widget.EventResult)]
    , _rgwPadding :: Widget.R
    }
Lens.makeLenses ''ResultGroupWidgets

extraSymbol :: Text
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
    HoleInfo m -> Maybe Sugar.EntityId -> Sugar.PickedResult -> T m PickedResult
afterPick holeInfo mFirstHoleInside pr =
    do
        Property.set (hiState holeInfo) HoleState.emptyState
        result
            & pickedEventResult . Widget.eCursor .~ Monoid.Last (Just cursorId)
            & return
    where
        result = eventResultOfPickedResult pr
        cursorId =
            mFirstHoleInside <&> WidgetIds.fromEntityId
            & fromMaybe myHoleId
            & result ^. pickedIdTranslations
        myHoleId =
            WidgetIds.fromEntityId $ hiEntityId holeInfo

fixNumWithDotEventMap ::
    Monad m =>
    HoleInfo m -> Sugar.HoleResult name m ->
    Widget.EventMap (T m Widget.EventResult)
fixNumWithDotEventMap holeInfo res
    | endsWithDot
    , Lens.has literalNum conv
    , Sugar.WrapAction wrap <- conv ^. hrWrapAction = mkAction wrap
    | endsWithDot
    , Lens.has (wrappedExpr . literalNum) conv
    , Sugar.WrapperAlready t <- conv ^. hrWrapAction = mkAction (return t)
    | otherwise = mempty
    where
        mkAction toHole =
            E.charGroup "Operator" doc operatorChars $
            \c ->
            do
                (uuid, entityId) <- toHole
                cursor <-
                    HoleState.setHoleStateAndJump uuid
                    (HoleState ("." <> Text.singleton c)) entityId
                return $ Widget.eventResultFromCursor cursor
        endsWithDot = "." `Text.isSuffixOf` HoleInfo.hiSearchTerm holeInfo
        doc = E.Doc ["Edit", "Apply Operator"]
        conv = res ^. Sugar.holeResultConverted
        literalNum = Sugar.rBody . Sugar._BodyLiteral . Sugar._LiteralNum
        wrappedExpr =
            Sugar.rBody . Sugar._BodyHole .
            Sugar.holeMArg . Lens._Just . Sugar.haExpr
        hrWrapAction = Sugar.rPayload . Sugar.plActions . Sugar.wrap

makeShownResult ::
    Monad m =>
    HoleInfo m -> Result m ->
    ExprGuiM m (Widget (T m Widget.EventResult), ShownResult m)
makeShownResult holeInfo result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- ExprGuiM.transaction $ rHoleResult result
        config <- Config.hole <$> ExprGuiM.readConfig
        (widget, mkEventMap) <- makeHoleResultWidget (rId result) res
        stdSpacing <- ExprGuiM.widgetEnv BWidgets.stdSpacing
        let padding = Config.holeResultPadding config <&> realToFrac & (* stdSpacing)
        let mFirstHoleInside =
                res ^? Sugar.holeResultConverted
                . SugarLens.holePayloads . Sugar.plEntityId
        return
            ( widget & Widget.pad padding
            , ShownResult
              { srMkEventMap =
                  mkEventMap <&> mappend (fixNumWithDotEventMap holeInfo res)
              , srHasHoles =
                  Lens.has (Sugar.holeResultConverted . SugarLens.holePayloads)
                  res
              , srPick =
                  res ^. Sugar.holeResultPick
                  >>= afterPick holeInfo mFirstHoleInside
              }
            )

makeExtraSymbol :: Monad m => AnimId -> Bool -> ResultsList n -> ExprGuiM m View
makeExtraSymbol animId isSelected results
    | Lens.nullOf (HoleResults.rlExtra . traverse) results = pure View.empty
    | otherwise =
        do
            Config.Hole{..} <- Config.hole <$> ExprGuiM.readConfig
            let extraSymbolColor
                    | isSelected = holeExtraSymbolColorSelected
                    | otherwise = holeExtraSymbolColorUnselected
            hSpace <- ExprGuiM.widgetEnv BWidgets.stdHSpaceWidth
            ExprGuiM.makeLabel extraSymbol animId
                <&> View.scale extraSymbolScaleFactor
                <&> View.tint extraSymbolColor
                <&> View.assymetricPad (Vector2 hSpace 0) 0

makeResultGroup ::
    Monad m =>
    HoleInfo m ->
    ResultsList m ->
    ExprGuiM m (ResultGroupWidgets m)
makeResultGroup holeInfo results =
    do
        Config.Hole{..} <- Config.hole <$> ExprGuiM.readConfig
        (mainResultWidget, shownMainResult) <-
            makeShownResult holeInfo mainResult
        let mainResultHeight = mainResultWidget ^. Widget.height
        let makeExtra =
                results ^. HoleResults.rlExtra
                & makeExtraResultsWidget holeInfo mainResultHeight
        (mSelectedResult, extraResWidget, extraPadding) <-
            if Widget.isFocused mainResultWidget
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
                    focusFirstExtraResult (results ^. HoleResults.rlExtra)
                    <&> ($ Widget.empty)
                    <&> \x -> (Nothing, x, 0)
        let isSelected = Lens.has Lens._Just mSelectedResult
        extraSymbolWidget <-
            makeExtraSymbol (Widget.toAnimId (rId mainResult)) isSelected results
            <&> Widget.fromView
        return ResultGroupWidgets
            { _rgwMainResult = shownMainResult
            , _rgwMSelectedResult = mSelectedResult
            , _rgwRow = [mainResultWidget, extraSymbolWidget, extraResWidget]
            , _rgwPadding = extraPadding
            }
    where
        mainResult = results ^. HoleResults.rlMain
        focusFirstExtraResult [] = return id
        focusFirstExtraResult (result:_) = makeFocusable (rId result)

makeExtraResultsWidget ::
    Monad m =>
    HoleInfo m -> Anim.R -> [Result m] ->
    ExprGuiM m (Maybe (ShownResult m), Widget (T m Widget.EventResult), Widget.R)
makeExtraResultsWidget _ _ [] = return (Nothing, Widget.empty, 0)
makeExtraResultsWidget holeInfo mainResultHeight extraResults@(firstResult:_) =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        let mkResWidget result =
                do
                    isOnResult <-
                        WE.isSubCursor (rId result)
                        & ExprGuiM.widgetEnv
                    (widget, shownResult) <- makeShownResult holeInfo result
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
                & addBackground (Widget.toAnimId (rId firstResult)) holeOpenBGColor
        return
            ( msum mResults
            , widget
                & Widget.size .~ Vector2 0 height
                & Widget.translate (Vector2 0 (0.5 * (height - headHeight)))
            , (widget ^. Widget.size . _2) - 0.5 * (headHeight + mainResultHeight)
                & max 0
            )

makeFocusable ::
    (Monad m, Applicative f) =>
    Widget.Id ->
    ExprGuiM m (Widget (f Widget.EventResult) -> Widget (f Widget.EventResult))
makeFocusable = ExprGuiM.widgetEnv . BWidgets.makeFocusableView

applyResultLayout ::
    Functor f => f (ExpressionGui m) -> f (AlignedWidget (T m Widget.EventResult))
applyResultLayout fGui =
    fGui <&> (^. TreeLayout.render)
    ?? TreeLayout.LayoutParams
        { _layoutMode = TreeLayout.LayoutWide
        , _layoutContext = TreeLayout.LayoutClear
        }

makeHoleResultWidget ::
    Monad m =>
    Widget.Id -> Sugar.HoleResult (Name m) m ->
    ExprGuiM m (Widget (T m Widget.EventResult), ExprGuiM m (Widget.EventMap (T m Widget.EventResult)))
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
            makeFocusable resultId <*> mkWidget
            <&> Widget.view . View.animFrames %~
                Anim.mapIdentities (<> (resultSuffix # Widget.toAnimId resultId))
        return (widget, mkEventMap)
    where
        mkWidget =
            holeResultConverted
            & postProcessSugar
            & ExprGuiM.makeSubexpression (const 0)
            & applyResultLayout
            <&> (^. AlignedWidget.widget)
        holeResultEntityId =
            holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
        idWithinResultWidget =
            holeResult
            ^? Sugar.holeResultConverted
            . SugarLens.holePayloads . Sugar.plEntityId
            & fromMaybe holeResultEntityId
            & WidgetIds.fromEntityId
        holeResultConverted = holeResult ^. Sugar.holeResultConverted

postProcessSugar :: ExpressionN m () -> ExpressionN m ExprGuiT.Payload
postProcessSugar expr =
    expr
    & Lens.mapped .~ pl
    & SugarLens.holeArgs . Sugar.plData . ExprGuiT.plShowAnnotation
    .~ ExprGuiT.alwaysShowAnnotations
    where
        pl =
            ExprGuiT.emptyPayload NearestHoles.none
            & ExprGuiT.plShowAnnotation .~ ExprGuiT.neverShowAnnotations

makeNoResults ::
    Monad m => AnimId -> ExprGuiM m (Widget (T m Widget.EventResult))
makeNoResults animId =
    ExpressionGui.makeLabel "(No results)" animId
    <&> (^. AlignedWidget.widget)

makeHiddenResultsMView ::
    Monad m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe View)
makeHiddenResultsMView HaveHiddenResults myId =
    Just <$> ExprGuiM.makeLabel "..." (Widget.toAnimId myId)
makeHiddenResultsMView NoHiddenResults _ = return Nothing

addMResultPicker :: Monad m => Maybe (ShownResult m) -> ExprGuiM m ()
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
    Monad m =>
    [ResultGroupWidgets m] -> HaveHiddenResults -> Widget.Id ->
    ExprGuiM m (Widget (T m Widget.EventResult))
layoutResults groups hiddenResults myId
    | null groups = makeNoResults (Widget.toAnimId myId)
    | otherwise =
        do
            hiddenResultsWidgets <-
                makeHiddenResultsMView hiddenResults myId <&> (^.. Lens._Just)
                <&> map Widget.fromView
            let grid =
                  rows
                  & Lens.mapped . Lens.mapped %~ (,) (Grid.Alignment (Vector2 0 0.5))
                  & Grid.make & snd
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
    Monad m => HoleInfo m ->
    [ResultsList m] -> HaveHiddenResults ->
    ExprGuiM m (Maybe (ShownResult m), Widget (T m Widget.EventResult))
makeResultsWidget holeInfo shownResultsLists hiddenResults =
    do
        groupsWidgets <- traverse (makeResultGroup holeInfo) shownResultsLists
        let mSelectedResult = groupsWidgets ^? Lens.traversed . rgwMSelectedResult . Lens._Just
        let mFirstResult = groupsWidgets ^? Lens.traversed . rgwMainResult
        let mResult = mSelectedResult <|> mFirstResult
        addMResultPicker mResult
        widget <- hiIds holeInfo & hidOpen & layoutResults groupsWidgets hiddenResults
        return (mResult, widget)

assignHoleEditCursor ::
    Monad m =>
    HoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
    ExprGuiM m a ->
    ExprGuiM m a
assignHoleEditCursor holeInfo shownMainResultsIds allShownResultIds searchTermId action =
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
        WidgetIds{..} = hiIds holeInfo
        destId
            | Text.null (HoleInfo.hiSearchTerm holeInfo) = searchTermId
            | otherwise = head (shownMainResultsIds ++ [searchTermId])

makeUnderCursorAssignment ::
    Monad m =>
    [ResultsList m] -> HaveHiddenResults ->
    HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeUnderCursorAssignment shownResultsLists hasHiddenResults holeInfo =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config

        (mShownResult, resultsWidget) <-
            makeResultsWidget holeInfo shownResultsLists hasHiddenResults

        (searchTermEventMap, resultsEventMap) <-
            EventMap.makeOpenEventMaps holeInfo mShownResult

        -- We make our own type view here instead of
        -- ExpressionGui.stdWrap, because we want to synchronize the
        -- active BG width with the inferred type width
        typeView <- TypeView.make (hiInferredType holeInfo) (Widget.toAnimId hidHole)

        vspace <- ExpressionGui.annotationSpacer
        hoverResultsWidget <-
            addDarkBackground (Widget.toAnimId hidResultsPrefix)
            ??
            ( resultsWidget
              & Widget.width %~ max (typeView ^. View.width)
              & Widget.strongerEvents resultsEventMap .
                addBackground (Widget.toAnimId hidResultsPrefix) holeOpenBGColor
              & AlignedWidget.fromCenteredWidget
              & AlignedWidget.addAfter AlignedWidget.Vertical
                [ vspace
                , Widget.fromView typeView & AlignedWidget.fromCenteredWidget
                ]
              & TreeLayout.fromAlignedWidget
            ) & applyResultLayout
            <&> (^. AlignedWidget.widget)
        searchTermGui <- SearchTerm.make holeInfo
        return $ TreeLayout.render # \layoutMode ->
            let w = layoutMode
                    & ( searchTermGui
                        & TreeLayout.widget %~
                          Widget.weakerEvents searchTermEventMap
                      ) ^. TreeLayout.render
            in
                w
                & AlignedWidget.addAfter AlignedWidget.Vertical
                    [ AlignedWidget.fromCenteredWidget hoverResultsWidget
                        & AlignedWidget.alignment . _1 .~ 0
                    ]
                & alignment .~ w ^. alignment
    where
        alignment :: Lens' (AlignedWidget f) (Vector2 Widget.R)
        alignment = AlignedWidget.absAlignedWidget . _1
        WidgetIds{..} = hiIds holeInfo

makeOpenSearchAreaGui ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeOpenSearchAreaGui pl holeInfo =
    do
        (shownResultsLists, hasHiddenResults) <-
            -- Don't generate results of open holes inside hole results
            if ExprGuiT.plOfHoleResult pl
            then return ([], HaveHiddenResults)
            else HoleResults.makeAll holeInfo
        let shownMainResultsIds =
                rId . (^. HoleResults.rlMain) <$> shownResultsLists
        let allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
        exprEventMap <- ExprEventMap.make pl
        makeUnderCursorAssignment shownResultsLists
            hasHiddenResults holeInfo
            & assignHoleEditCursor holeInfo shownMainResultsIds
              allShownResultIds (holeInfo & hiIds & hidOpenSearchTerm)
            <&> TreeLayout.widget %~ Widget.weakerEvents exprEventMap
