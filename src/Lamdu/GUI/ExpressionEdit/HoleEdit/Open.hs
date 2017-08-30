{-# LANGUAGE TemplateHaskell, PatternGuards, NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, DeriveTraversable #-}
-- | The search area (search term + results) of an open/active hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.Open
    ( makeOpenSearchAreaGui
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.List.Lens (suffixed)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, EventResult, AnimId)
import           GUI.Momentu.Align (Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.CharClassification (charPrecedence, operatorChars)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultsList(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as HoleResults
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
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

data PickedResult = PickedResult
    { _pickedEventResult :: Widget.EventResult
    , _pickedIdTranslations :: Widget.Id -> Widget.Id
    }
Lens.makeLenses ''PickedResult

resultSuffix :: Lens.Prism' AnimId AnimId
resultSuffix = suffixed ["result suffix"]

data ResultGroup m = ResultGroup
    { _rgOption :: !(Menu.Option (T m))
    , _rgPickEventMap :: !(Widget.EventMap (T m Widget.EventResult))
    }
Lens.makeLenses ''ResultGroup

makeShownResult ::
    Monad m =>
    HoleInfo m -> Result m ->
    ExprGuiM m
    ( Widget.EventMap (T m Widget.EventResult)
    , WithTextPos (Widget (T m Widget.EventResult))
    )
makeShownResult holeInfo result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- rHoleResult result & transaction
        theme <- Theme.hole <$> Lens.view Theme.theme
        stdSpacing <- Spacer.getSpaceSize
        let padding = Theme.holeResultPadding theme <&> realToFrac & (* stdSpacing)
        makeHoleResultWidget holeInfo (rId result) res <&> _2 %~ Element.pad padding

makeResultGroup ::
    Monad m =>
    HoleInfo m ->
    ResultsList m ->
    ExprGuiM m (ResultGroup m)
makeResultGroup holeInfo results =
    do
        (pickMain, mainResultWidget) <- makeShownResult holeInfo mainResult
        let mainFocused = Widget.isFocused (mainResultWidget ^. Align.tValue)
        cursorOnExtra <-
            Widget.isSubCursor
            ?? results ^. HoleResults.rlExtraResultsPrefixId
        let isSelected = mainFocused || cursorOnExtra
        extraResWidget <-
            if isSelected
            then makeExtra
            else focusFirstExtraResult (results ^. HoleResults.rlExtra)
        return ResultGroup
            { _rgOption = Menu.Option
                { Menu._oId = rId (results ^. HoleResults.rlMain)
                , Menu._oWidget = mainResultWidget
                , Menu._oSubmenuWidget = extraResWidget
                }
            , _rgPickEventMap = pickMain
            }
    where
        mainResult = results ^. HoleResults.rlMain
        makeExtra = makeExtraResultsWidget holeInfo (results ^. HoleResults.rlExtra)
        focusFirstExtraResult [] = return Nothing
        focusFirstExtraResult (result:_) =
            Widget.makeFocusableView ?? rId result ?? Element.empty
            <&> Align.WithTextPos 0 <&> Just

makeExtraResultsWidget ::
    Monad m =>
    HoleInfo m -> [Result m] ->
    ExprGuiM m (Maybe (WithTextPos (Widget (T m Widget.EventResult))))
makeExtraResultsWidget _ [] = return Nothing
makeExtraResultsWidget holeInfo extraResults@(firstResult:_) =
    do
        theme <- Lens.view Theme.theme
        traverse (makeShownResult holeInfo) extraResults
            <&> map snd
            <&> Glue.vbox
            <&> addBackground (Widget.toAnimId (rId firstResult)) (Theme.hoverBGColor theme)
            <&> Just

applyResultLayout ::
    Functor f => f (ExpressionGui m) -> f (WithTextPos (Widget (T m Widget.EventResult)))
applyResultLayout fGui =
    fGui <&> (^. Responsive.render)
    ?? Responsive.LayoutParams
        { Responsive._layoutMode = Responsive.LayoutWide
        , Responsive._layoutContext = Responsive.LayoutClear
        }

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
        mapPrefix = foldr ((.) . reprefix) id
        reprefix (old, new) ident =
            WidgetId.subId old ident & maybe ident (WidgetId.joinId new)
        pickedResultAnimIdTranslation idTranslations =
            -- Map only the first anim id component
            Lens.ix 0 %~ \x -> fromMaybe x $ Map.lookup x idMap
            where
                idMap =
                    idTranslations
                    & Lens.traversed . Lens.both %~
                      head . Widget.toAnimId . WidgetIds.fromEntityId
                    & Map.fromList

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

-- | Remove unwanted event handlers from a hole result
removeUnwanted :: Monad m => ExprGuiM m (Widget.EventMap a -> Widget.EventMap a)
removeUnwanted =
    do
        config <- Lens.view Config.config
        minOpPrec <- ExprGuiM.readMinOpPrec
        let unwantedKeys =
                concat
                [ Config.delKeys config
                , Grid.stdKeys ^.. traverse
                , Config.letAddItemKeys config
                ]
                <&> MetaKey.toModKey
        let disallowedOperator '.' = False
            disallowedOperator char
                | char `notElem` operatorChars = False
                | otherwise = charPrecedence char < minOpPrec
        return (E.filterChars (not . disallowedOperator) . deleteKeys unwantedKeys)
    where
        deleteKeys = E.deleteKeys . map (E.KeyEvent MetaKey.KeyState'Pressed)

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
        endsWithDot = "." `Text.isSuffixOf` hiSearchTerm holeInfo
        doc = E.Doc ["Edit", "Apply Operator"]
        conv = res ^. Sugar.holeResultConverted
        literalNum = Sugar.rBody . Sugar._BodyLiteral . Sugar._LiteralNum
        wrappedExpr =
            Sugar.rBody . Sugar._BodyHole .
            Sugar.holeKind . Sugar._WrapperHole . Sugar.haExpr
        hrWrapAction = Sugar.rPayload . Sugar.plActions . Sugar.wrap

makeHoleResultWidget ::
    Monad m =>
    HoleInfo m -> Widget.Id -> Sugar.HoleResult (Name m) m ->
    ExprGuiM m (Widget.EventMap (T m Widget.EventResult), WithTextPos (Widget (T m Widget.EventResult)))
makeHoleResultWidget holeInfo resultId holeResult =
    do
        remUnwanted <- removeUnwanted
        holeConfig <- Lens.view Config.config <&> Config.hole
        let pickAndMoveToNextHole =
                Widget.keysEventMapMovesCursor (Config.holePickAndMoveToNextHoleKeys holeConfig)
                    (E.Doc ["Edit", "Result", "Pick and move to next hole"]) .
                pure . WidgetIds.fromEntityId
        let pickEventMap =
                -- TODO: Does this entityId business make sense?
                case hiNearestHoles holeInfo ^. NearestHoles.next of
                Just nextHoleEntityId | Lens.has Lens._Nothing mFirstHoleInside ->
                    simplePickRes (Config.holePickResultKeys holeConfig) <>
                    pickAndMoveToNextHole nextHoleEntityId
                _ ->
                    simplePickRes (mappend Config.holePickResultKeys Config.holePickAndMoveToNextHoleKeys holeConfig)
                <&> pickBefore
        isSelected <- Widget.isSubCursor ?? resultId
        when isSelected (ExprGuiM.setResultPicker (pickBefore (pure mempty)))
        holeResultConverted
            & postProcessSugar
            & ExprGuiM.makeSubexpression
            <&> Widget.enterResultCursor .~ resultId
            <&> E.eventMap %~ remUnwanted
            <&> E.eventMap %~ mappend (fixNumWithDotEventMap holeInfo holeResult)
            <&> E.eventMap . E.emDocs . E.docStrs . Lens._last %~ (<> " (On picked result)")
            <&> E.eventMap . Lens.mapped %~ pickBefore
            <&> E.eventMap %~ mappend pickEventMap
            & Widget.assignCursor resultId idWithinResultWidget
            & applyResultLayout
            <&> fixFocalArea
            <&> Element.setLayers . Element.layers . Lens.traverse %~
                Anim.mapIdentities (<> (resultSuffix # Widget.toAnimId resultId))
            <&> (,) pickEventMap
    where
        fixFocalArea =
            Align.tValue . Widget.sizedState <. Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .@~
            (:[]) . Rect 0
        holeResultEntityId = holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
        mFirstHoleInside = holeResult ^? Sugar.holeResultConverted . SugarLens.holePayloads . Sugar.plEntityId
        idWithinResultWidget = fromMaybe holeResultEntityId mFirstHoleInside & WidgetIds.fromEntityId
        holeResultConverted = holeResult ^. Sugar.holeResultConverted
        pickBefore action =
            do
                pickedResult <-
                    holeResult ^. Sugar.holeResultPick
                    >>= afterPick holeInfo resultId mFirstHoleInside
                action
                    <&> Widget.eCursor . Lens._Wrapped' . Lens.mapped %~
                        pickedResult ^. pickedIdTranslations
                    <&> mappend (pickedResult ^. pickedEventResult)
        simplePickRes keys =
            Widget.keysEventMap keys (E.Doc ["Edit", "Result", "Pick"]) (return ())

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

emptyPickEventMap ::
    (Monad m, Applicative f) => ExprGuiM m (Widget.EventMap (f Widget.EventResult))
emptyPickEventMap =
    Lens.view Config.config <&> Config.hole <&> keys <&> mkEventMap
    where
        keys c = Config.holePickResultKeys c ++ Config.holePickAndMoveToNextHoleKeys c
        mkEventMap k =
            Widget.keysEventMap k (E.Doc ["Edit", "Result", "Pick (N/A)"]) (pure ())

makeResultsWidget ::
    Monad m =>
    Widget.R -> HoleInfo m -> [ResultsList m] -> Menu.HasMoreOptions ->
    ExprGuiM m (Widget.EventMap (T m Widget.EventResult), Menu.OrderedOptions (Widget (T m Widget.EventResult)))
makeResultsWidget minWidth holeInfo shownResultsLists hiddenResults =
    do
        groupsWidgets <- traverse (makeResultGroup holeInfo) shownResultsLists
        pickResultEventMap <-
            case groupsWidgets of
            [] -> emptyPickEventMap
            (x:_) -> x ^. rgPickEventMap & return
        theme <- Lens.view Theme.theme
        Menu.layout minWidth (groupsWidgets <&> (^. rgOption)) hiddenResults
            <&> Lens.mapped %~
                addBackground (Widget.toAnimId (hidResultsPrefix hids))
                (Theme.hoverBGColor theme)
            <&> (,) pickResultEventMap
    where
        hids = hiIds holeInfo

assignHoleEditCursor ::
    Monad m =>
    HoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
    ExprGuiM m a ->
    ExprGuiM m a
assignHoleEditCursor holeInfo shownMainResultsIds allShownResultIds searchTermId action =
    do
        let sub x = Widget.isSubCursor ?? x
        shouldBeOnResult <- sub (hidResultsPrefix hids)
        isOnResult <- traverse sub allShownResultIds <&> or
        let assignSource
                | shouldBeOnResult && not isOnResult =
                      Reader.local (Widget.cursor .~ destId)
                | otherwise =
                      Widget.assignCursor (hidOpen hids) destId
        assignSource action
    where
        hids = hiIds holeInfo
        destId = head (shownMainResultsIds ++ [searchTermId])

resultsHoverOptions ::
    Functor f =>
    Menu.Placement ->
    (Widget (f EventResult) -> Widget (f EventResult)) ->
    (Widget (f EventResult) -> Widget (f EventResult)) ->
    Menu.OrderedOptions (Widget (f EventResult)) ->
    Hover.AnchoredWidget (f EventResult) ->
    [Hover.AnchoredWidget (f EventResult)]
resultsHoverOptions pos addBg addBgAnnotation results searchTerm =
    case pos of
    Menu.Above -> [ above, aboveLeft ]
    Menu.AnyPlace ->
        [ below
        , above
        , belowLeft
        , aboveLeft
        , Aligned 0.5 annotatedTerm /|/ Aligned 0.5 (bgResults ^. Menu.optionsFromTop)
            ^. Align.value
        ]
    Menu.Below ->
        [ below
        , belowLeft
        , Aligned 1 annotatedTerm /|/ Aligned 1 (bgResults ^. Menu.optionsFromBottom)
            ^. Align.value
        , Aligned 1 (bgResults ^. Menu.optionsFromBottom) /|/ Aligned 1 annotatedTerm
            ^. Align.value
        ]
    where
        above = (bgResults ^. Menu.optionsFromBottom) /-/ annotatedTerm
        aboveLeft =
            Aligned 1 (bgResults ^. Menu.optionsFromBottom)
            /-/ Aligned 1 annotatedTerm
            ^. Align.value
        below = searchTerm /-/ addBgAnnotation (results ^. Menu.optionsFromTop)
        belowLeft =
            Aligned 1 searchTerm
            /-/ Aligned 1 (addBgAnnotation (results ^. Menu.optionsFromTop))
            ^. Align.value
        bgResults = results <&> addBg
        annotatedTerm = searchTerm & Widget.widget %~ addBgAnnotation

makeUnderCursorAssignment ::
    Monad m =>
    [ResultsList m] -> Menu.HasMoreOptions -> HoleInfo m ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m Widget.EventResult)))
makeUnderCursorAssignment shownResultsLists hasHiddenResults holeInfo =
    do
        -- We make our own type view here instead of
        -- ExpressionGui.stdWrap, because we want to synchronize the
        -- active BG width with the inferred type width
        typeView <-
            TypeView.make (hiInferredType holeInfo) (Widget.toAnimId (hidHole hids))
            <&> (^. Align.tValue)

        searchTermEventMap <- EventMap.makeOpenEventMap holeInfo

        (pickFirstResult, resultsWidgets) <-
            makeResultsWidget (typeView ^. Element.width) holeInfo shownResultsLists hasHiddenResults
            <&> _2 . Lens.mapped %~ E.strongerEvents searchTermEventMap

        vspace <- ExpressionGui.annotationSpacer
        addBg <- addDarkBackground (Widget.toAnimId (hidResultsPrefix hids))
        resBg <-
            addDarkBackground (Widget.toAnimId (hidResultsPrefix hids) ++ ["results"])
        let addAnnotation x = addBg (x /-/ vspace /-/ typeView)
        searchTermWidget <-
            SearchTerm.make holeInfo
            <&> Align.tValue %~ Hover.anchor . E.weakerEvents (searchTermEventMap <> pickFirstResult)
        return $
            \placement ->
            searchTermWidget
            & Align.tValue %~
                Hover.hoverInPlaceOf
                (resultsHoverOptions placement resBg addAnnotation resultsWidgets
                    (searchTermWidget ^. Align.tValue))
    where
        hids = hiIds holeInfo

makeOpenSearchAreaGui ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> HoleInfo m ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m Widget.EventResult)))
makeOpenSearchAreaGui pl holeInfo =
    do
        (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll holeInfo
        let shownMainResultsIds = shownResultsLists <&> rId . (^. HoleResults.rlMain)
        let allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
        exprEventMap <- ExprEventMap.make pl ExprGuiM.NoHolePick
        delKeys <- Config.delKeys
        let unwrapAsDelEventMap =
                hiHole holeInfo ^? Sugar.holeKind . Sugar._WrapperHole . Sugar.haUnwrap . Sugar._UnwrapAction
                & maybe mempty
                    ( Widget.keysEventMapMovesCursor delKeys
                        (E.Doc ["Edit", "Unwrap"])
                        . fmap WidgetIds.fromEntityId
                    )
        makeUnderCursorAssignment shownResultsLists
            hasHiddenResults holeInfo
            & assignHoleEditCursor holeInfo shownMainResultsIds
              allShownResultIds (holeInfo & hiIds & hidOpenSearchTerm)
            <&> Lens.mapped . Align.tValue %~ E.weakerEvents (mappend unwrapAsDelEventMap exprEventMap)
