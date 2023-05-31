{-# LANGUAGE TypeFamilies, TupleSections #-}

module Lamdu.GUI.Annotation
    ( annotationSpacer
    , NeighborVals(..)
    , EvalAnnotationOptions(..), maybeAddAnnotationWith

    , addInferredType, shrinkValAnnotationsIfNeeded

    , PostProcessAnnotation, WhichAnnotation(..), ShrinkRatio
    , postProcessAnnotationFromSelected

    , addAnnotationBackground -- used for open holes
    , maybeAddAnnotationPl
    , addAnnotationBelow
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader, pushToReaderExt)
import           Data.CurAndPrev (CurAndPrev(..), CurPrevTag(..), curPrevTag, fallbackToPrev)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.ValAnnotation (ValAnnotation)
import qualified Lamdu.Config.Theme.ValAnnotation as ValAnnotation
import qualified Lamdu.GUI.EvalView as EvalView
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

addAnnotationBackgroundH :: _ => Lens.ALens' ValAnnotation Draw.Color -> m (a -> a)
addAnnotationBackgroundH color =
    do
        t <- Lens.view has
        Draw.backgroundColor (t ^# Theme.valAnnotation . color) & pushToReader

addAnnotationBackground :: _ => m (a -> a)
addAnnotationBackground = addAnnotationBackgroundH ValAnnotation.valAnnotationBGColor

addAnnotationHoverBackground :: _ => m (a -> a)
addAnnotationHoverBackground = addAnnotationBackgroundH ValAnnotation.valAnnotationHoverBGColor

data WhichAnnotation = TypeAnnotation | ValAnnotation

type ShrinkRatio = M.Vector2 Widget.R

type PostProcessAnnotation m = WhichAnnotation -> m (ShrinkRatio -> M.View -> M.View)

postProcessAnnotationFromSelected :: _ => Bool -> PostProcessAnnotation m
postProcessAnnotationFromSelected False = shrinkIfNeeded
postProcessAnnotationFromSelected True = hoverWideAnnotation

shrinkValAnnotationsIfNeeded :: _ => PostProcessAnnotation m
shrinkValAnnotationsIfNeeded TypeAnnotation = addAnnotationBackground <&> const
shrinkValAnnotationsIfNeeded ValAnnotation = shrinkIfNeeded ValAnnotation

shrinkIfNeeded :: _ => PostProcessAnnotation m
shrinkIfNeeded _ =
    addAnnotationBackground
    <&>
    \addBg shrinkRatio view ->
    M.scale shrinkRatio view & addBg

hoverWideAnnotation :: _ => PostProcessAnnotation m
hoverWideAnnotation which =
    do
        shrinker <- shrinkIfNeeded which
        addBg <- addAnnotationHoverBackground
        pad <- pushToReaderExt (pushToReaderExt pushToReader) Element.pad
        pure $
            \shrinkRatio wideView ->
                let shrunkView = shrinker shrinkRatio wideView
                -- TODO: This is a buggy hover that ignores
                -- Surrounding (and exits screen).
                in
                addBg wideView
                & pad 0 (shrunkView ^. View.vSize - wideView ^. View.vSize)
                & Element.hoverLayeredImage

processAnnotationGui :: _ => (ShrinkRatio -> M.View -> M.View) -> m (Widget.R -> M.View -> M.View)
processAnnotationGui postProcess =
    f
    <$> Lens.view (has . Theme.valAnnotation)
    <*> Spacer.getSpaceSize
    where
        f th stdSpacing minWidth ann
            | annWidth > minWidth + max shrinkAtLeast expansionLimit
            || heightShrinkRatio < 1 =
                postProcess shrinkRatio ann
            | otherwise =
                maybeTooNarrow ann & postProcess 1.0
            where
                annWidth = ann ^. M.width
                expansionLimit = th ^. ValAnnotation.valAnnotationWidthExpansionLimit
                maxWidth = minWidth + expansionLimit
                shrinkAtLeast = th ^. ValAnnotation.valAnnotationShrinkAtLeast
                heightShrinkRatio =
                    th ^. ValAnnotation.valAnnotationMaxHeight * stdSpacing ^. _2
                    / ann ^. M.height
                shrinkRatio =
                    annWidth - shrinkAtLeast & min maxWidth & max minWidth
                    & (/ annWidth) & min heightShrinkRatio & pure
                maybeTooNarrow
                    | minWidth > annWidth =
                        M.padAround (M.Vector2 ((minWidth - annWidth) / 2) 0)
                    | otherwise = id

data EvalResDisplay name = EvalResDisplay
    { erdSource :: CurPrevTag
    , erdVal :: Annotated Sugar.EntityId # Sugar.Result name
    }

makeEvaluationResultView :: _ => EvalResDisplay Name -> m (M.WithTextPos M.View)
makeEvaluationResultView res =
    do
        th <- Lens.view has
        EvalView.make (erdVal res)
            <&>
            case erdSource res of
            Current -> id
            Prev -> M.tint (th ^. Theme.eval . Theme.staleResultTint)

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable, Show)

neighborPositions ::
    ( Element.SizedElement evalView
    , Element.SizedElement prevView
    , Element.SizedElement nextView
    , MonadReader env m, Has Dir.Layout env
    ) =>
    evalView -> prevView -> nextView -> m (M.Vector2 Widget.R, M.Vector2 Widget.R)
neighborPositions evalView prev next =
    Lens.view has <&>
    \case
    Dir.LeftToRight -> f (-prev ^. Element.width) rightX
    Dir.RightToLeft -> f rightX (-next ^. Element.width)
    where
        f prevX nextX =
            ( M.Vector2 prevX prevY
            , M.Vector2 nextX nextY
            )
        baseline = 0.5 * evalView ^. Element.height
        prevY = baseline - prev ^. Element.height
        nextY = baseline
        rightX = evalView ^. Element.width

makeEvalView ::
    _ =>
    Maybe (NeighborVals (Maybe (EvalResDisplay Name))) ->
    EvalResDisplay Name -> m (M.WithTextPos M.View)
makeEvalView mNeighbours evalRes =
    do
        evalTheme <- Lens.view (has . Theme.eval)
        let neighbourView n =
                Lens._Just makeEvaluationResultViewBG n
                <&> Lens.mapped %~ M.scale (evalTheme ^. Theme.neighborsScaleFactor)
                <&> Lens.mapped %~ M.padAround (evalTheme ^. Theme.neighborsPadding)
                <&> fromMaybe M.empty
        (prev, next) <-
            case mNeighbours of
            Nothing -> pure (M.empty, M.empty)
            Just (NeighborVals mPrev mNext) ->
                (,)
                <$> neighbourView mPrev
                <*> neighbourView mNext
        evalView <- makeEvaluationResultView evalRes
        (prevPos, nextPos) <- neighborPositions evalView prev next
        evalView
            & Element.setLayeredImage <>~ Element.translateLayeredImage prevPos (prev ^. View.vAnimLayers)
            & Element.setLayeredImage <>~ Element.translateLayeredImage nextPos (next ^. View.vAnimLayers)
            & pure
    where
        evalElemId erd =
            erdVal erd ^. annotation & WidgetIds.fromEntityId & M.asElemId
        makeEvaluationResultViewBG res =
            ( addAnnotationBackground
            & local (M.elemIdPrefix .~ evalElemId res)
            ) <*> makeEvaluationResultView res
            <&> (^. Align.tValue)

annotationSpacer :: _ => m M.View
annotationSpacer =
    Lens.view (has . Theme.valAnnotation . ValAnnotation.valAnnotationSpacing)
    >>= Spacer.vspaceLines

addAnnotationBelow ::
    _ => (ShrinkRatio -> M.View -> M.View) -> M.WithTextPos M.View -> m (M.Widget f -> M.Widget f)
addAnnotationBelow postProcess annView =
    do
        vspace <- annotationSpacer
        processAnn <- processAnnotationGui postProcess
        padToSize <- pushToReaderExt (pushToReaderExt pushToReader) Element.padToSize
        let ann w =
                processAnn (w ^. M.width) (annView ^. M.tValue)
                & padToSize (M.Vector2 theMinWidth 0) 0
                where
                    theMinWidth = w ^. M.width
        (|---|) <- Glue.mkGlue Glue.Vertical & pushToReaderExt pushToReader
        pure (\w -> w |---| vspace |---| ann w)

addInferredType ::
    _ =>
    Annotated Sugar.EntityId # Sugar.Type Name -> (ShrinkRatio -> M.View -> M.View) ->
    m (M.Widget f -> M.Widget f)
addInferredType t s = TypeView.make t >>= addAnnotationBelow s

addEvaluationResult ::
    _ =>
    Maybe (NeighborVals (Maybe (EvalResDisplay Name))) ->
    EvalResDisplay Name -> PostProcessAnnotation m ->
    m (M.Widget f -> M.Widget f)
addEvaluationResult mNeigh resDisp postProcess =
    case erdVal resDisp ^. hVal of
    Sugar.RRecord [] -> Styled.addBgColor Theme.evaluatedPathBGColor & pushToReader
    Sugar.RFunc _ -> pure id
    _ -> addAnnotationBelow <$> postProcess ValAnnotation <*> makeEvalView mNeigh resDisp & join

maybeAddAnnotationPl ::
    _ =>
    Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) o1 ->
    GuiM env i o (M.Widget o -> M.Widget o)
maybeAddAnnotationPl pl =
    do
        postProcessAnnotation <- isExprSelected <&> postProcessAnnotationFromSelected
        maybeAddAnnotation postProcessAnnotation
            (pl ^. Sugar.plAnnotation)
            & local (M.elemIdPrefix .~ elemId)
    where
        isExprSelected = GuiState.isSubCursor (WidgetIds.fromExprPayload pl)
        elemId = WidgetIds.fromExprPayload pl & M.asElemId

data EvalAnnotationOptions
    = NormalEvalAnnotation
    | WithNeighbouringEvalAnnotations (NeighborVals (Maybe Sugar.BinderParamScopeId))
    deriving Show

getAnnotationMode ::
    Monad i =>
    EvalAnnotationOptions -> Sugar.EvaluationScopes name i ->
    GuiM env i o (Maybe (EvalResDisplay name, Maybe (NeighborVals (Maybe (EvalResDisplay name)))))
getAnnotationMode opt ann =
    do
        neighbourVals <-
            case opt of
            NormalEvalAnnotation -> pure Nothing
            WithNeighbouringEvalAnnotations neighbors ->
                neighbors & traverse . Lens._Just %%~
                    GuiM.im . valOfScopePreferCur ann . (^. Sugar.bParamScopeId)
                <&> traverse %~ join
                <&> Just
        GuiM.readMScopeId
            >>= GuiM.im . valOfScope ann
            <&> Lens.mapped %~ (, neighbourVals)

maybeAddAnnotationWith ::
    _ =>
    EvalAnnotationOptions -> PostProcessAnnotation (GuiM env i o) ->
    Sugar.Annotation (Sugar.EvaluationScopes Name i) Name ->
    GuiM env i o (M.Widget o -> M.Widget o)
maybeAddAnnotationWith opt postProcessAnnotation ann =
    case ann of
    Sugar.AnnotationNone -> pure id
    Sugar.AnnotationType typ -> postProcessAnnotation TypeAnnotation >>= addInferredType typ
    Sugar.AnnotationVal val -> maybeAddValAnnotationWith opt postProcessAnnotation val

maybeAddValAnnotationWith ::
    _ =>
    EvalAnnotationOptions -> PostProcessAnnotation (GuiM env i o) ->
    Sugar.EvaluationScopes Name i ->
    GuiM env i o (M.Widget o -> M.Widget o)
maybeAddValAnnotationWith opt postProcessAnnotation ann =
    getAnnotationMode opt ann
    >>=
    \case
    Nothing -> pure id
    Just (scopeAndVal, mNeighborVals) ->
        addEvaluationResult mNeighborVals scopeAndVal postProcessAnnotation

maybeAddAnnotation ::
    _ =>
    PostProcessAnnotation (GuiM env i o) ->
    Sugar.Annotation (Sugar.EvaluationScopes Name i) Name ->
    GuiM env i o (M.Widget o -> M.Widget o)
maybeAddAnnotation = maybeAddAnnotationWith NormalEvalAnnotation

valOfScope ::
    Applicative i =>
    Sugar.EvaluationScopes name i -> CurAndPrev (Maybe Sugar.ScopeId) ->
    i (Maybe (EvalResDisplay name))
valOfScope curPrevAnn mScopeIds =
    go
    <$> curPrevTag
    <*> curPrevAnn
    <*> mScopeIds
    & fallbackToPrev
    & sequenceA
    where
        go _ _ Nothing = Nothing
        go tag ann (Just scopeId) =
            ann ^? Lens._Just . Lens.ix scopeId <&> Lens.mapped %~ EvalResDisplay tag

valOfScopePreferCur ::
    Applicative i =>
    Sugar.EvaluationScopes name i -> Sugar.ScopeId ->
    i (Maybe (EvalResDisplay name))
valOfScopePreferCur ann = valOfScope ann . pure . Just
