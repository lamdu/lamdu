{-# LANGUAGE TemplateHaskell, PatternGuards, NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TypeFamilies #-}
-- | The search area (search term + results) of an open/active hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchAreaGui
    , ResultsPlacement(..)
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad (msum)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.List.Lens (suffixed)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Align (Aligned(..), WithTextPos(..), AnchoredWidget)
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View, (/-/), (/|/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget(..), EventResult)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.Id as WidgetId
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Lamdu.CharClassification (operatorChars)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
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
import           Lamdu.GUI.Hover (addBackground, addDarkBackground)
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
    , _rgwMainResultWidget :: WithTextPos (Widget (T m Widget.EventResult))
    , _rgwExtraResultSymbol :: WithTextPos View
    , _rgwExtraResultsWidget ::  Widget (T m Widget.EventResult)
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
        , Widget._eVirtualCursor = Monoid.Last Nothing
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
    HoleInfo m -> Widget.Id -> Maybe Sugar.EntityId ->
    Sugar.PickedResult -> T m PickedResult
afterPick holeInfo resultId mFirstHoleInside pr =
    do
        Property.set (hiState holeInfo) HoleState.emptyState
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
            WidgetIds.fromEntityId $ hiEntityId holeInfo
        obliterateOtherResults animId =
            case animId ^? resultSuffix of
            Nothing -> animId
            Just unsuffixed
                | Lens.has (suffixed (Widget.toAnimId resultId)) unsuffixed ->
                      animId
                | otherwise -> "obliterated" : animId

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
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)), ShownResult m)
makeShownResult holeInfo result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- rHoleResult result & transaction
        theme <- Theme.hole <$> Lens.view Theme.theme
        (widget, mkEventMap) <- makeHoleResultWidget (rId result) res
        stdSpacing <- Spacer.getSpaceSize
        let padding = Theme.holeResultPadding theme <&> realToFrac & (* stdSpacing)
        let mFirstHoleInside =
                res ^? Sugar.holeResultConverted
                . SugarLens.holePayloads . Sugar.plEntityId
        return
            ( View.pad padding widget
            , ShownResult
              { srMkEventMap =
                  mkEventMap <&> mappend (fixNumWithDotEventMap holeInfo res)
              , srHasHoles =
                  Lens.has (Sugar.holeResultConverted . SugarLens.holePayloads)
                  res
              , srPick =
                  res ^. Sugar.holeResultPick
                  >>= afterPick holeInfo (rId result) mFirstHoleInside
              }
            )

makeExtraSymbol :: Monad m => Bool -> ResultsList n -> ExprGuiM m (WithTextPos View)
makeExtraSymbol isSelected results
    | Lens.nullOf (HoleResults.rlExtra . traverse) results = pure View.empty
    | otherwise =
        do
            Theme.Hole{..} <- Theme.hole <$> Lens.view Theme.theme
            let extraSymbolColor
                    | isSelected = holeExtraSymbolColorSelected
                    | otherwise = holeExtraSymbolColorUnselected
            hSpace <- Spacer.getSpaceSize <&> (^. _1)
            TextView.makeLabel extraSymbol
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
        Config.Hole{..} <- Config.hole <$> Lens.view Config.config
        (mainResultWidget, shownMainResult) <- makeShownResult holeInfo mainResult
        (mSelectedResult, extraResWidget) <-
            if Widget.isFocused (mainResultWidget ^. Align.tValue)
            then do
                (_, extraResWidget) <- makeExtra
                return (Just shownMainResult, extraResWidget)
            else do
                cursorOnExtra <-
                    Widget.isSubCursor
                    ?? results ^. HoleResults.rlExtraResultsPrefixId
                if cursorOnExtra
                    then makeExtra
                    else
                    results ^. HoleResults.rlExtra
                    & focusFirstExtraResult
                    <&> \x -> (Nothing, x)
        let isSelected = Lens.has Lens._Just mSelectedResult
        extraSymbolWidget <-
            makeExtraSymbol isSelected results
            & Reader.local (View.animIdPrefix .~ Widget.toAnimId (rId mainResult))
        return ResultGroupWidgets
            { _rgwMainResult = shownMainResult
            , _rgwMSelectedResult = mSelectedResult
            , _rgwMainResultWidget = mainResultWidget
            , _rgwExtraResultSymbol = extraSymbolWidget
            , _rgwExtraResultsWidget = extraResWidget
            }
    where
        mainResult = results ^. HoleResults.rlMain
        makeExtra =
            makeExtraResultsWidget holeInfo (results ^. HoleResults.rlExtra)
            <&> Lens.mapped %~ (^. Align.tValue)
        focusFirstExtraResult [] = return View.empty
        focusFirstExtraResult (result:_) = Widget.makeFocusableView ?? rId result ?? View.empty

makeExtraResultsWidget ::
    Monad m =>
    HoleInfo m -> [Result m] ->
    ExprGuiM m
    ( Maybe (ShownResult m)
    , WithTextPos (Widget (T m Widget.EventResult))
    )
makeExtraResultsWidget _ [] = return (Nothing, View.empty)
makeExtraResultsWidget holeInfo extraResults@(firstResult:_) =
    do
        theme <- Lens.view Theme.theme
        let mkResWidget result =
                do
                    isOnResult <- Widget.isSubCursor ?? rId result
                    (widget, shownResult) <- makeShownResult holeInfo result
                    return
                        ( shownResult <$ guard isOnResult
                        , widget
                        )
        (mResults, widgets) <- traverse mkResWidget extraResults <&> unzip
        return
            ( msum mResults
            , View.vbox widgets
                & addBackground (Widget.toAnimId (rId firstResult))
                  (Theme.hoverBGColor theme)
            )

applyResultLayout ::
    Functor f => f (ExpressionGui m) -> f (WithTextPos (Widget (T m Widget.EventResult)))
applyResultLayout fGui =
    fGui <&> (^. TreeLayout.render)
    ?? TreeLayout.LayoutParams
        { _layoutMode = TreeLayout.LayoutWide
        , _layoutContext = TreeLayout.LayoutClear
        }

makeHoleResultWidget ::
    Monad m =>
    Widget.Id -> Sugar.HoleResult (Name m) m ->
    ExprGuiM m
    ( WithTextPos (Widget (T m Widget.EventResult))
    , ExprGuiM m (Widget.EventMap (T m Widget.EventResult))
    )
makeHoleResultWidget resultId holeResult =
    do
        Theme.Hole{..} <- Lens.view Theme.theme <&> Theme.hole
        let mkEventMap =
                -- Create a hidden result widget that we never display, but only
                -- keep the event map from. We always tell it that it has focus,
                -- so that even if we're on the search term, we can have valid
                -- event maps of any result (we actually use the first one's
                -- event map)
                Reader.local (Widget.cursor .~ idWithinResultWidget) mkWidget
                <&> getEvents
        widget <-
            (Widget.makeFocusableView ?? resultId <&> (Align.tValue %~))
            <*> mkWidget
            <&> View.setLayers . View.layers . Lens.traverse %~
                Anim.mapIdentities (<> (resultSuffix # Widget.toAnimId resultId))
        return (widget, mkEventMap)
    where
        getEvents widget =
            case widget ^. Align.tValue . Widget.wState of
            Widget.StateUnfocused {} -> mempty
            Widget.StateFocused makeFocus ->
                let focus = makeFocus (Widget.Surrounding 0 0 0 0)
                in
                (focus ^. Widget.fEventMap)
                (Widget.VirtualCursor (last (focus ^. Widget.fFocalAreas)))
        mkWidget =
            holeResultConverted
            & postProcessSugar
            & ExprGuiM.makeSubexpression
            & applyResultLayout
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

makeNoResults :: Monad m => ExprGuiM m (WithTextPos View)
makeNoResults = TextView.makeLabel "(No results)"

makeHiddenResultsMView ::
    Monad m => HaveHiddenResults -> ExprGuiM m (Maybe (WithTextPos View))
makeHiddenResultsMView NoHiddenResults = return Nothing
makeHiddenResultsMView HaveHiddenResults = TextView.makeLabel "..." <&> Just

addMResultPicker :: Monad m => Maybe (ShownResult m) -> ExprGuiM m ()
addMResultPicker mSelectedResult =
    case mSelectedResult of
    Nothing -> return ()
    Just res -> ExprGuiM.setResultPicker $ (^. pickedEventResult) <$> srPick res

layoutResults ::
    Monad m =>
    [ResultGroupWidgets m] -> HaveHiddenResults ->
    ExprGuiM m (Widget (T m Widget.EventResult))
layoutResults groups hiddenResults
    | null groups = makeNoResults <&> (^. Align.tValue) <&> Widget.fromView
    | otherwise =
        do
            hiddenResultsWidget <-
                makeHiddenResultsMView hiddenResults
                <&> maybe View.empty (^. Align.tValue)
            View.vbox (groups <&> layoutGroup) /-/ hiddenResultsWidget & EventMap.blockDownEvents & return
    where
        layoutGroup group =
            Align.hoverInPlaceOf
            (Align.hoverBesideOptionsAxis View.Horizontal (group ^. rgwExtraResultsWidget) base)
            base
            where
                base =
                    ((group ^. rgwMainResultWidget & View.width .~ maxMainResultWidth)
                        /|/ (group ^. rgwExtraResultSymbol)) ^. Align.tValue & Align.anchor
        maxMainResultWidth = groups ^.. Lens.traversed . rgwMainResultWidget . View.width & maximum

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
        widget <- layoutResults groupsWidgets hiddenResults
        return (mResult, widget)

assignHoleEditCursor ::
    Monad m =>
    HoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
    ExprGuiM m a ->
    ExprGuiM m a
assignHoleEditCursor holeInfo shownMainResultsIds allShownResultIds searchTermId action =
    do
        let sub x = Widget.subId ?? x <&> isJust
        shouldBeOnResult <- sub hidResultsPrefix
        isOnResult <- traverse sub allShownResultIds <&> or
        let assignSource
                | shouldBeOnResult && not isOnResult =
                      Reader.local (Widget.cursor .~ destId)
                | otherwise =
                      Widget.assignCursor hidOpen destId
        assignSource action
    where
        WidgetIds{..} = hiIds holeInfo
        destId
            | Text.null (HoleInfo.hiSearchTerm holeInfo) = searchTermId
            | otherwise = head (shownMainResultsIds ++ [searchTermId])

data ResultsPlacement = Above | Below | AnyPlace

resultsHoverOptions ::
    Functor f =>
    ResultsPlacement ->
    (Widget (f EventResult) -> Widget (f EventResult)) -> (Widget (f EventResult) -> Widget (f EventResult)) ->
    Widget (f EventResult) -> AnchoredWidget (f EventResult) -> [AnchoredWidget (f EventResult)]
resultsHoverOptions pos addBg addAnnotation results searchTerm =
    case pos of
    Above -> [ above, aboveLeft ]
    AnyPlace ->
        [ below
        , above
        , belowLeft
        , aboveLeft
        , Aligned 0.5 annotatedTerm /|/ Aligned 0.5 bgResults ^. Align.value
        ]
    Below ->
        [ below
        , belowLeft
        , Aligned 1 annotatedTerm /|/ Aligned 1 bgResults ^. Align.value
        , Aligned 1 bgResults /|/ Aligned 1 annotatedTerm ^. Align.value
        ]
    where
        above = bgResults /-/ annotatedTerm
        aboveLeft = Aligned 1 bgResults /-/ Aligned 1 annotatedTerm ^. Align.value
        below = searchTerm /-/ addAnnotation results
        belowLeft = Aligned 1 searchTerm /-/ Aligned 1 (addAnnotation results) ^. Align.value
        bgResults = addBg results
        annotatedTerm = searchTerm & Widget.widget %~ addAnnotation

makeUnderCursorAssignment ::
    Monad m =>
    [ResultsList m] -> HaveHiddenResults -> HoleInfo m ->
    ExprGuiM m (ResultsPlacement -> WithTextPos (Widget (T m Widget.EventResult)))
makeUnderCursorAssignment shownResultsLists hasHiddenResults holeInfo =
    do
        theme <- Lens.view Theme.theme

        (mShownResult, resultsWidget) <-
            makeResultsWidget holeInfo shownResultsLists hasHiddenResults

        (searchTermEventMap, resultsEventMap) <-
            EventMap.makeOpenEventMaps holeInfo mShownResult

        -- We make our own type view here instead of
        -- ExpressionGui.stdWrap, because we want to synchronize the
        -- active BG width with the inferred type width
        typeView <- TypeView.make (hiInferredType holeInfo) (Widget.toAnimId hidHole) <&> (^. Align.tValue)

        vspace <- ExpressionGui.annotationSpacer
        let widget =
                resultsWidget
                & View.width %~ max (typeView ^. View.width)
                & addBackground (Widget.toAnimId hidResultsPrefix) (Theme.hoverBGColor theme)
                & E.strongerEvents resultsEventMap
        addBg <- addDarkBackground (Widget.toAnimId hidResultsPrefix)
        resBg <- addDarkBackground (Widget.toAnimId hidResultsPrefix ++ ["results"])
        let addAnnotation x = addBg (x /-/ vspace /-/ typeView)
        searchTermWidget <-
            SearchTerm.make holeInfo
            <&> Align.tValue %~ Align.anchor . E.weakerEvents searchTermEventMap
        return $
            \placement ->
            searchTermWidget
            & Align.tValue %~
                Align.hoverInPlaceOf
                (resultsHoverOptions placement resBg addAnnotation widget (searchTermWidget ^. Align.tValue))
    where
        WidgetIds{..} = hiIds holeInfo

makeOpenSearchAreaGui ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> HoleInfo m ->
    ExprGuiM m (ResultsPlacement -> WithTextPos (Widget (T m Widget.EventResult)))
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
        exprEventMap <- ExprEventMap.make pl ExprGuiM.NoHolePick
        makeUnderCursorAssignment shownResultsLists
            hasHiddenResults holeInfo
            & assignHoleEditCursor holeInfo shownMainResultsIds
              allShownResultIds (holeInfo & hiIds & hidOpenSearchTerm)
            <&> Lens.mapped . Align.tValue %~ E.weakerEvents exprEventMap
