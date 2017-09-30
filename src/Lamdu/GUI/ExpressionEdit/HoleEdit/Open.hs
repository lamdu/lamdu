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
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as EventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiSearchTermProperty)
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
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..), ExpressionN)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Parens.Add as AddParens
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
    { _rgOption :: !(Menu.Option (ExprGuiM m) (T m Widget.EventResult))
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
        return ResultGroup
            { _rgOption = Menu.Option
                { Menu._oId = results ^. HoleResults.rlExtraResultsPrefixId
                , Menu._oWidget = mainResultWidget
                , Menu._oSubmenuWidgets =
                    if null extras
                    then Menu.SubmenuEmpty
                    else Menu.SubmenuItems (traverse (makeShownResult holeInfo) extras <&> map snd)
                }
            , _rgPickEventMap = pickMain
            }
    where
        extras = results ^. HoleResults.rlExtra
        mainResult = results ^. HoleResults.rlMain

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
removeUnwanted :: Monad m => Int -> ExprGuiM m (Widget.EventMap a -> Widget.EventMap a)
removeUnwanted minOpPrec =
    do
        config <- Lens.view Config.config
        let unwantedKeys =
                concat
                [ Config.delKeys config
                , Grid.stdKeys ^.. traverse
                , Config.letAddItemKeys config
                ]
                <&> MetaKey.toModKey
        let disallowedOperator '.' = False
            disallowedOperator char
                | char `notElem` Chars.operator = False
                | otherwise = Chars.precedence char < minOpPrec
        return (E.filterChars (not . disallowedOperator) . deleteKeys unwantedKeys)
    where
        deleteKeys = E.deleteKeys . map (E.KeyEvent MetaKey.KeyState'Pressed)

fixNumWithDotEventMap ::
    Monad m =>
    HoleInfo m -> Sugar.HoleResult (T m) (Sugar.Expression name (T m) ()) ->
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
            E.charGroup "Operator" doc Chars.operator $
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
    HoleInfo m -> Widget.Id ->
    Sugar.HoleResult (T m) (Sugar.Expression (Name m) (T m) ()) ->
    ExprGuiM m
    ( Widget.EventMap (T m Widget.EventResult)
    , WithTextPos (Widget (T m Widget.EventResult))
    )
makeHoleResultWidget holeInfo resultId holeResult =
    do
        remUnwanted <- removeUnwanted (hiMinOpPrec holeInfo)
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
    & AddParens.add
    <&> pl
    & SugarLens.holeArgs . Sugar.plData . ExprGuiT.plShowAnnotation
    .~ ExprGuiT.alwaysShowAnnotations
    where
        pl (minOpPrec, needParens, ()) =
            ExprGuiT.Payload
            { ExprGuiT._plStoredEntityIds = []
            , ExprGuiT._plNearestHoles = NearestHoles.none
            , ExprGuiT._plShowAnnotation = ExprGuiT.neverShowAnnotations
            , ExprGuiT._plNeedParens = needParens == AddParens.NeedsParens
            , ExprGuiT._plMinOpPrec = minOpPrec
            }

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
    ExprGuiM m (Widget.EventMap (T m Widget.EventResult), Hover.Ordered (Widget (T m Widget.EventResult)))
makeResultsWidget minWidth holeInfo shownResultsLists hiddenResults =
    do
        groupsWidgets <- traverse (makeResultGroup holeInfo) shownResultsLists
        pickResultEventMap <-
            case groupsWidgets of
            [] -> emptyPickEventMap
            (x:_) -> x ^. rgPickEventMap & return
        Menu.layout minWidth (groupsWidgets <&> (^. rgOption)) hiddenResults
            <&> (,) pickResultEventMap

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
        destId
            | Text.null (hiSearchTerm holeInfo) = searchTermId
            | otherwise = head (shownMainResultsIds ++ [searchTermId])

resultsHoverOptions ::
    ( MonadReader env m, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , Functor f
    ) =>
    m
    (Menu.Placement ->
     (Widget (f EventResult) -> Widget (f EventResult)) ->
     Hover.Ordered (Widget (f EventResult)) ->
     Hover.AnchoredWidget (f EventResult) ->
     [Hover.AnchoredWidget (f EventResult)])
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
    [ResultsList m] -> Menu.HasMoreOptions -> HoleInfo m ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m Widget.EventResult)))
makeUnderCursorAssignment shownResultsLists hasHiddenResults holeInfo =
    do
        -- We make our own type view here instead of
        -- ExpressionGui.stdWrap, because we want to synchronize the
        -- active BG width with the inferred type width
        typeView <-
            ExpressionGui.addAnnotationBackground holeAnimId
            <*> TypeView.make (hiInferredType holeInfo) holeAnimId
            <&> (^. Align.tValue)

        searchTermEventMap <- EventMap.makeOpenEventMap holeInfo <&> disallowFirstOperatorChar

        (pickFirstResult, resultsWidgets) <-
            makeResultsWidget (typeView ^. Element.width) holeInfo shownResultsLists hasHiddenResults
            <&> _2 . Lens.mapped %~ E.strongerEvents searchTermEventMap

        vspace <- ExpressionGui.annotationSpacer
        let addAnnotation x = x /-/ vspace /-/ typeView
        searchTermWidget <-
            SearchTerm.make holeInfo
            <&> Align.tValue %~ Hover.anchor . E.weakerEvents pickFirstResult
        mkOptions <- resultsHoverOptions & Reader.local (Element.animIdPrefix .~ WidgetId.toAnimId (hidOpen hids))
        return $
            \placement ->
            searchTermWidget
            & Align.tValue %~
                Hover.hoverInPlaceOf
                (mkOptions placement addAnnotation resultsWidgets
                    (searchTermWidget ^. Align.tValue))
    where
        holeAnimId = hidHole hids & Widget.toAnimId
        hids = hiIds holeInfo
        disallowFirstOperatorChar
            | Text.null searchTerm = E.filterChars (`notElem` Chars.operator)
            | otherwise = id
        searchTerm = hiSearchTermProperty holeInfo ^. Property.pVal

makeOpenSearchAreaGui ::
    Monad m =>
    HoleInfo m ->
    ExprGuiM m (Menu.Placement -> WithTextPos (Widget (T m Widget.EventResult)))
makeOpenSearchAreaGui holeInfo =
    do
        (shownResultsLists, hasHiddenResults) <- HoleResults.makeAll holeInfo
        let shownMainResultsIds = shownResultsLists <&> rId . (^. HoleResults.rlMain)
        let allShownResultIds =
                [ rId . (^. HoleResults.rlMain)
                , (^. HoleResults.rlExtraResultsPrefixId)
                ] <*> shownResultsLists
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
            <&> Lens.mapped . Align.tValue %~ E.weakerEvents unwrapAsDelEventMap
