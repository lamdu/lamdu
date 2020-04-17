{-# LANGUAGE TypeFamilies, TupleSections #-}
module Lamdu.GUI.ExpressionGui.Annotation
    ( annotationSpacer
    , NeighborVals(..)
    , EvalAnnotationOptions(..), maybeAddAnnotationWith

    , addInferredType, shrinkValAnnotationsIfNeeded

    , PostProcessAnnotation, WhichAnnotation(..), ShrinkRatio
    , postProcessAnnotationFromSelected

    , evaluationResult
    , addAnnotationBackground -- used for open holes
    , maybeAddAnnotationPl
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurAndPrev(..), CurPrevTag(..), curPrevTag, fallbackToPrev)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.ValAnnotation (ValAnnotation)
import qualified Lamdu.Config.Theme.ValAnnotation as ValAnnotation
import qualified Lamdu.GUI.EvalView as EvalView
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

addAnnotationBackgroundH ::
    (MonadReader env m, Has Theme env, Element a, Element.HasAnimIdPrefix env) =>
    Lens.ALens' ValAnnotation Draw.Color -> m (a -> a)
addAnnotationBackgroundH color =
    do
        t <- Lens.view has
        bgAnimId <- Element.subAnimId ?? ["annotation background"]
        Draw.backgroundColor bgAnimId (t ^# Theme.valAnnotation . color) & pure

addAnnotationBackground ::
    (MonadReader env m, Has Theme env, Element a, Element.HasAnimIdPrefix env) =>
    m (a -> a)
addAnnotationBackground = addAnnotationBackgroundH ValAnnotation.valAnnotationBGColor

addAnnotationHoverBackground ::
    (MonadReader env m, Has Theme env, Element a, Element.HasAnimIdPrefix env) => m (a -> a)
addAnnotationHoverBackground = addAnnotationBackgroundH ValAnnotation.valAnnotationHoverBGColor

data WhichAnnotation = TypeAnnotation | ValAnnotation

type ShrinkRatio = Vector2 Widget.R

type PostProcessAnnotation m = WhichAnnotation -> m (ShrinkRatio -> View -> View)

postProcessAnnotationFromSelected ::
    (MonadReader env m, Has Theme env, Has Dir.Layout env, Element.HasAnimIdPrefix env) =>
    Bool -> PostProcessAnnotation m
postProcessAnnotationFromSelected False = shrinkIfNeeded
postProcessAnnotationFromSelected True = hoverWideAnnotation

shrinkValAnnotationsIfNeeded ::
    (MonadReader env m, Has Theme env, Element.HasAnimIdPrefix env) =>
    PostProcessAnnotation m
shrinkValAnnotationsIfNeeded TypeAnnotation = addAnnotationBackground <&> const
shrinkValAnnotationsIfNeeded ValAnnotation = shrinkIfNeeded ValAnnotation

shrinkIfNeeded ::
    (MonadReader env m, Has Theme env, Element.HasAnimIdPrefix env) =>
    PostProcessAnnotation m
shrinkIfNeeded _ =
    addAnnotationBackground
    <&>
    \addBg shrinkRatio view ->
    Element.scale shrinkRatio view & addBg

hoverWideAnnotation ::
    (MonadReader env m, Has Theme env, Has Dir.Layout env, Element.HasAnimIdPrefix env) =>
    PostProcessAnnotation m
hoverWideAnnotation which =
    do
        shrinker <- shrinkIfNeeded which
        addBg <- addAnnotationHoverBackground
        pad <- Element.pad
        pure $
            \shrinkRatio wideView ->
                let shrunkView = shrinker shrinkRatio wideView
                -- TODO: This is a buggy hover that ignores
                -- Surrounding (and exits screen).
                in
                addBg wideView
                & pad 0 (shrunkView ^. View.vSize - wideView ^. View.vSize)
                & Element.hoverLayeredImage

processAnnotationGui ::
    (MonadReader env m, Has Theme env, Spacer.HasStdSpacing env) =>
    m (ShrinkRatio -> View -> View) -> m (Widget.R -> View -> View)
processAnnotationGui postProcessAnnotation =
    f
    <$> Lens.view (has . Theme.valAnnotation)
    <*> Spacer.getSpaceSize
    <*> postProcessAnnotation
    where
        f th stdSpacing postProcess minWidth ann
            | annWidth > minWidth + max shrinkAtLeast expansionLimit
            || heightShrinkRatio < 1 =
                postProcess shrinkRatio ann
            | otherwise =
                maybeTooNarrow ann & postProcess 1.0
            where
                annWidth = ann ^. Element.width
                expansionLimit = th ^. ValAnnotation.valAnnotationWidthExpansionLimit
                maxWidth = minWidth + expansionLimit
                shrinkAtLeast = th ^. ValAnnotation.valAnnotationShrinkAtLeast
                heightShrinkRatio =
                    th ^. ValAnnotation.valAnnotationMaxHeight * stdSpacing ^. _2
                    / ann ^. Element.height
                shrinkRatio =
                    annWidth - shrinkAtLeast & min maxWidth & max minWidth
                    & (/ annWidth) & min heightShrinkRatio & pure
                maybeTooNarrow
                    | minWidth > annWidth =
                        Element.padAround (Vector2 ((minWidth - annWidth) / 2) 0)
                    | otherwise = id

data EvalResDisplay name = EvalResDisplay
    { erdSource :: CurPrevTag
    , erdVal :: Sugar.ResVal name
    }

makeEvaluationResultView ::
    (Monad i, Has (Texts.Name Text) env) =>
    EvalResDisplay Name ->
    GuiM env i o (WithTextPos View)
makeEvaluationResultView res =
    do
        th <- Lens.view has
        EvalView.make (erdVal res)
            <&>
            case erdSource res of
            Current -> id
            Prev -> Element.tint (th ^. Theme.eval . Theme.staleResultTint)

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable)

makeEvalView ::
    (Monad i, Has (Texts.Name Text) env) =>
    Maybe (NeighborVals (Maybe (EvalResDisplay Name))) ->
    EvalResDisplay Name -> GuiM env i o (WithTextPos View)
makeEvalView mNeighbours evalRes =
    do
        evalTheme <- Lens.view (has . Theme.eval)
        let neighbourView n =
                Lens._Just makeEvaluationResultViewBG n
                <&> Lens.mapped %~ Element.scale (evalTheme ^. Theme.neighborsScaleFactor)
                <&> Lens.mapped %~ Element.padAround (evalTheme ^. Theme.neighborsPadding)
                <&> fromMaybe Element.empty
        (prev, next) <-
            case mNeighbours of
            Nothing -> pure (Element.empty, Element.empty)
            Just (NeighborVals mPrev mNext) ->
                (,)
                <$> neighbourView mPrev
                <*> neighbourView mNext
        evalView <- makeEvaluationResultView evalRes
        let prevPos = Vector2 0 0.5 * evalView ^. Element.size - prev ^. Element.size
        let nextPos = Vector2 1 0.5 * evalView ^. Element.size
        evalView
            & Element.setLayeredImage <>~ Element.translateLayeredImage prevPos (prev ^. View.vAnimLayers)
            & Element.setLayeredImage <>~ Element.translateLayeredImage nextPos (next ^. View.vAnimLayers)
            & pure
    where
        evalAnimId erd =
            erdVal erd ^. Sugar.resPayload & WidgetIds.fromEntityId & Widget.toAnimId
        makeEvaluationResultViewBG res =
            ( addAnnotationBackground
            & Reader.local (Element.animIdPrefix .~ evalAnimId res)
            ) <*> makeEvaluationResultView res
            <&> (^. Align.tValue)

annotationSpacer ::
    (MonadReader env m, Has Theme env, Has TextView.Style env) => m View
annotationSpacer =
    Lens.view (has . Theme.valAnnotation . ValAnnotation.valAnnotationSpacing)
    >>= Spacer.vspaceLines

addAnnotationH ::
    ( Functor f, MonadReader env m, Has Theme env, Has Dir.Layout env
    , Spacer.HasStdSpacing env
    ) =>
    m (WithTextPos View) ->
    m (ShrinkRatio -> View -> View) ->
    m (Widget f -> Widget f)
addAnnotationH f postProcess =
    do
        vspace <- annotationSpacer
        annotationLayout <- f <&> (^. Align.tValue)
        processAnn <- processAnnotationGui postProcess
        padToSize <- Element.padToSize
        let ann w =
                processAnn (w ^. Element.width) annotationLayout
                & padToSize (Vector2 theMinWidth 0) 0
                where
                    theMinWidth = w ^. Element.width
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        pure (\w -> w |---| vspace |---| ann w)

addInferredType ::
    ( Functor f, MonadReader env m, Spacer.HasStdSpacing env, Has Theme env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env
    , Has (Texts.Code Text) env, Has (Texts.Name Text) env
    ) =>
    Annotated Sugar.EntityId # Sugar.Type Name -> PostProcessAnnotation m ->
    m (Widget f -> Widget f)
addInferredType typ postProcess =
    addAnnotationH (TypeView.make typ) (postProcess TypeAnnotation)

addEvaluationResult ::
    (Monad i, Functor f, Has (Texts.Name Text) env) =>
    Maybe (NeighborVals (Maybe (EvalResDisplay Name))) ->
    EvalResDisplay Name -> PostProcessAnnotation (GuiM env i o) ->
    GuiM env i o (Widget f -> Widget f)
addEvaluationResult mNeigh resDisp postProcess =
    case erdVal resDisp ^. Sugar.resBody of
    Sugar.RRecord (Sugar.ResRecord []) -> Styled.addBgColor Theme.evaluatedPathBGColor
    Sugar.RFunc _ -> pure id
    _ -> addAnnotationH (makeEvalView mNeigh resDisp) (postProcess ValAnnotation)

maybeAddAnnotationPl ::
    ( Monad i, Monad o, Glue.HasTexts env, Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    ) =>
    Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o1 -> GuiM env i o (Widget o -> Widget o)
maybeAddAnnotationPl pl =
    do
        postProcessAnnotation <-
            if pl ^. Sugar.plNeverShrinkTypeAnnotations
            then pure shrinkValAnnotationsIfNeeded
            else isExprSelected <&> postProcessAnnotationFromSelected
        maybeAddAnnotation postProcessAnnotation
            (pl ^. Sugar.plAnnotation)
            & Reader.local (Element.animIdPrefix .~ animId)
    where
        isExprSelected = GuiState.isSubCursor ?? WidgetIds.fromExprPayload pl
        animId = WidgetIds.fromExprPayload pl & Widget.toAnimId

evaluationResult ::
    Monad i =>
    Sugar.Payload (Sugar.EvaluationScopes name i) name i o ->
    GuiM env i o (Maybe (Sugar.ResVal name))
evaluationResult pl =
    do
        scopeId <- GuiM.readMScopeId
        case pl ^? Sugar.plAnnotation . Sugar._AnnotationVal of
            Nothing -> pure Nothing
            Just x -> valOfScope x scopeId & GuiM.im <&> Lens._Just %~ erdVal

data EvalAnnotationOptions
    = NormalEvalAnnotation
    | WithNeighbouringEvalAnnotations (NeighborVals (Maybe Sugar.BinderParamScopeId))

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
                -- neighbors <&> (>>= valOfScopePreferCur ann . (^. Sugar.bParamScopeId))
                -- & Just
                neighbors & traverse . Lens._Just %%~
                    GuiM.im . valOfScopePreferCur ann . (^. Sugar.bParamScopeId)
                <&> traverse %~ join
                <&> Just
        GuiM.readMScopeId
            >>= GuiM.im . valOfScope ann
            <&> Lens.mapped %~ (, neighbourVals)

maybeAddAnnotationWith ::
    ( Monad i, Monad o, Glue.HasTexts env
    , Has (Texts.Code Text) env, Has (Texts.Name Text) env
    ) =>
    EvalAnnotationOptions -> PostProcessAnnotation (GuiM env i o) ->
    Sugar.Annotation (Sugar.EvaluationScopes Name i) Name ->
    GuiM env i o (Widget o -> Widget o)
maybeAddAnnotationWith opt postProcessAnnotation ann =
    case ann of
    Sugar.AnnotationNone -> pure id
    Sugar.AnnotationType typ -> addInferredType typ postProcessAnnotation
    Sugar.AnnotationVal val -> maybeAddValAnnotationWith opt postProcessAnnotation val

maybeAddValAnnotationWith ::
    ( Monad i, Monad o
    , Has (Texts.Name Text) env
    ) =>
    EvalAnnotationOptions -> PostProcessAnnotation (GuiM env i o) ->
    Sugar.EvaluationScopes Name i ->
    GuiM env i o (Widget o -> Widget o)
maybeAddValAnnotationWith opt postProcessAnnotation ann =
    getAnnotationMode opt ann
    >>=
    \case
    Nothing -> pure id
    Just (scopeAndVal, mNeighborVals) ->
        addEvaluationResult mNeighborVals scopeAndVal postProcessAnnotation

maybeAddAnnotation ::
    ( Monad i, Monad o, Glue.HasTexts env, Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    ) =>
    PostProcessAnnotation (GuiM env i o) ->
    Sugar.Annotation (Sugar.EvaluationScopes Name i) Name ->
    GuiM env i o (Widget o -> Widget o)
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
