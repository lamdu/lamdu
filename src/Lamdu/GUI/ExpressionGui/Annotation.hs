{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeFamilies, LambdaCase, DeriveTraversable, FlexibleContexts #-}
module Lamdu.GUI.ExpressionGui.Annotation
    ( annotationSpacer
    , NeighborVals(..)
    , EvalAnnotationOptions(..), maybeAddAnnotationWith
    , WideAnnotationBehavior(..), wideAnnotationBehaviorFromSelected
    , evaluationResult
    , addAnnotationBackground -- used for open holes
    , maybeAddAnnotationPl
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction)
import           Data.Binary.Utils (encodeS)
import           Data.CurAndPrev (CurAndPrev(..), CurPrevTag(..), curPrevTag, fallbackToPrev)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/-/))
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config.Theme (HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.GUI.CodeEdit.AnnotationMode as AnnotationMode
import qualified Lamdu.GUI.EvalView as EvalView
import           Lamdu.GUI.ExpressionGui (ShowAnnotation(..), EvalModeShow(..))
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

addAnnotationBackgroundH ::
    (MonadReader env m, HasTheme env, Element a, Element.HasAnimIdPrefix env) =>
    (Theme.ValAnnotation -> Draw.Color) -> m (a -> a)
addAnnotationBackgroundH getColor =
    do
        t <- Lens.view theme
        bgAnimId <- Element.subAnimId ["annotation background"]
        Draw.backgroundColor bgAnimId (getColor (Theme.valAnnotation t)) & pure

addAnnotationBackground ::
    (MonadReader env m, HasTheme env, Element a, Element.HasAnimIdPrefix env) =>
    m (a -> a)
addAnnotationBackground = addAnnotationBackgroundH Theme.valAnnotationBGColor

addAnnotationHoverBackground ::
    (MonadReader env m, HasTheme env, Element a, Element.HasAnimIdPrefix env) => m (a -> a)
addAnnotationHoverBackground = addAnnotationBackgroundH Theme.valAnnotationHoverBGColor

data WideAnnotationBehavior
    = ShrinkWideAnnotation
    | HoverWideAnnotation
    | KeepWideTypeAnnotation

wideAnnotationBehaviorFromSelected :: Bool -> WideAnnotationBehavior
wideAnnotationBehaviorFromSelected False = ShrinkWideAnnotation
wideAnnotationBehaviorFromSelected True = HoverWideAnnotation

-- NOTE: Also adds the background color, because it differs based on
-- whether we're hovering
applyWideAnnotationBehavior ::
    (MonadReader env m, HasTheme env, Element.HasAnimIdPrefix env) =>
    WideAnnotationBehavior ->
    m (Vector2 Widget.R -> View -> View)
applyWideAnnotationBehavior KeepWideTypeAnnotation =
    addAnnotationBackground <&> const
applyWideAnnotationBehavior ShrinkWideAnnotation =
    addAnnotationBackground
    <&>
    \addBg shrinkRatio layout ->
    Element.scale shrinkRatio layout & addBg
applyWideAnnotationBehavior HoverWideAnnotation =
    do
        shrinker <- applyWideAnnotationBehavior ShrinkWideAnnotation
        addBg <- addAnnotationHoverBackground
        pure $
            \shrinkRatio layout ->
                -- TODO: This is a buggy hover that ignores
                -- Surrounding (and exits screen).
                shrinker shrinkRatio layout
                & Element.setLayers . Element.layers .~ addBg layout ^. View.vAnimLayers . Element.layers
                & Element.hoverLayers

processAnnotationGui ::
    ( MonadReader env m, HasTheme env, Spacer.HasStdSpacing env
    , Element.HasAnimIdPrefix env
    ) =>
    WideAnnotationBehavior ->
    m (Widget.R -> View -> View)
processAnnotationGui wideAnnotationBehavior =
    f
    <$> (Lens.view theme <&> Theme.valAnnotation)
    <*> addAnnotationBackground
    <*> Spacer.getSpaceSize
    <*> applyWideAnnotationBehavior wideAnnotationBehavior
    where
        f th addBg stdSpacing applyWide minWidth annotation
            | annotationWidth > minWidth + max shrinkAtLeast expansionLimit
            || heightShrinkRatio < 1 =
                applyWide shrinkRatio annotation
            | otherwise =
                maybeTooNarrow annotation & addBg
            where
                annotationWidth = annotation ^. Element.width
                expansionLimit =
                    Theme.valAnnotationWidthExpansionLimit th
                maxWidth = minWidth + expansionLimit
                shrinkAtLeast = Theme.valAnnotationShrinkAtLeast th
                heightShrinkRatio =
                    Theme.valAnnotationMaxHeight th * stdSpacing ^. _2
                    / annotation ^. Element.height
                shrinkRatio =
                    annotationWidth - shrinkAtLeast & min maxWidth & max minWidth
                    & (/ annotationWidth) & min heightShrinkRatio & pure
                maybeTooNarrow
                    | minWidth > annotationWidth = Element.pad (Vector2 ((minWidth - annotationWidth) / 2) 0)
                    | otherwise = id

data EvalResDisplay = EvalResDisplay
    { erdScope :: ER.ScopeId
    , erdSource :: CurPrevTag
    , erdVal :: ER.Val Type
    }

makeEvaluationResultView :: Monad m => EvalResDisplay -> ExprGuiM m (WithTextPos View)
makeEvaluationResultView res =
    do
        th <- Lens.view theme
        EvalView.make (erdVal res)
            <&>
            case erdSource res of
            Current -> id
            Prev -> Element.tint (Theme.staleResultTint (Theme.eval th))

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable)

makeEvalView ::
    Monad m =>
    Maybe (NeighborVals (Maybe EvalResDisplay)) -> EvalResDisplay ->
    ExprGuiM m (WithTextPos View)
makeEvalView mNeighbours evalRes =
    do
        evalTheme <- Lens.view theme <&> Theme.eval
        let animIdSuffix res =
                -- When we can scroll between eval view results we
                -- must encode the scope into the anim ID for smooth
                -- scroll to work.
                -- When we cannot, we'd rather not animate changes
                -- within a scrolled scope (use same animId).
                case mNeighbours of
                Nothing -> ["eval-view"]
                Just _ -> [encodeS (erdScope res)]
        let makeEvaluationResultViewBG res =
                addAnnotationBackground
                <*> makeEvaluationResultView res
                <&> (^. Align.tValue)
                & Reader.local (Element.animIdPrefix <>~ animIdSuffix res)
        let neighbourView n =
                Lens._Just makeEvaluationResultViewBG n
                <&> Lens.mapped %~ Element.scale (Theme.neighborsScaleFactor evalTheme)
                <&> Lens.mapped %~ Element.pad (Theme.neighborsPadding evalTheme)
                <&> fromMaybe Element.empty
        (prev, next) <-
            case mNeighbours of
            Nothing -> pure (Element.empty, Element.empty)
            Just (NeighborVals mPrev mNext) ->
                (,)
                <$> neighbourView mPrev
                <*> neighbourView mNext
        evalView <-
            makeEvaluationResultView evalRes
            & Reader.local (Element.animIdPrefix <>~ animIdSuffix evalRes)
        let prevPos = Vector2 0 0.5 * evalView ^. Element.size - prev ^. Element.size
        let nextPos = Vector2 1 0.5 * evalView ^. Element.size
        evalView
            & Element.setLayers <>~ Element.translateLayers prevPos (prev ^. View.vAnimLayers)
            & Element.setLayers <>~ Element.translateLayers nextPos (next ^. View.vAnimLayers)
            & pure

annotationSpacer ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env) => m View
annotationSpacer =
    Lens.view Theme.theme
    <&> Theme.valAnnotation
    <&> Theme.valAnnotationSpacing
    >>= Spacer.vspaceLines

addAnnotationH ::
    ( Functor f, MonadReader env m, HasTheme env
    , Spacer.HasStdSpacing env, Element.HasAnimIdPrefix env
    ) =>
    m (WithTextPos View) ->
    WideAnnotationBehavior ->
    m
    ((Widget.R -> Widget.R) ->
     Responsive (f GuiState.Update) ->
     Responsive (f GuiState.Update))
addAnnotationH f wideBehavior =
    do
        vspace <- annotationSpacer
        annotationLayout <- f <&> (^. Align.tValue)
        processAnn <- processAnnotationGui wideBehavior
        let onAlignedWidget minWidth w =
                w /-/ vspace /-/
                (processAnn (w ^. Element.width) annotationLayout
                    & Element.width %~ max (minWidth (w ^. Element.width)))
        pure $ \minWidth ->
            Responsive.alignedWidget %~ onAlignedWidget minWidth

addInferredType ::
    ( Functor f, MonadReader env m, Spacer.HasStdSpacing env, HasTheme env
    , MonadTransaction n m, Element.HasAnimIdPrefix env
    ) =>
    Type -> WideAnnotationBehavior ->
    m (Responsive (f GuiState.Update) ->
       Responsive (f GuiState.Update))
addInferredType typ wideBehavior =
    addAnnotationH (TypeView.make typ) wideBehavior ?? const 0

addEvaluationResult ::
    (Functor f, Monad m) =>
    Maybe (NeighborVals (Maybe EvalResDisplay)) -> EvalResDisplay ->
    WideAnnotationBehavior ->
    ExprGuiM m
    ((Widget.R -> Widget.R) ->
     Responsive (f GuiState.Update) ->
     Responsive (f GuiState.Update))
addEvaluationResult mNeigh resDisp wideBehavior =
    case (erdVal resDisp ^. ER.payload, erdVal resDisp ^. ER.body) of
    (T.TRecord T.CEmpty, _) ->
        Styled.addBgColor Theme.evaluatedPathBGColor <&> const
    (_, ER.RFunc{}) -> pure (flip const)
    _ ->
        case wideBehavior of
        KeepWideTypeAnnotation -> ShrinkWideAnnotation
        _ -> wideBehavior
        & addAnnotationH (makeEvalView mNeigh resDisp)

maybeAddAnnotationPl ::
    (Functor f, Monad m) =>
    Sugar.Payload name x ExprGui.Payload ->
    ExprGuiM m (Responsive (f GuiState.Update) -> Responsive (f GuiState.Update))
maybeAddAnnotationPl pl =
    do
        wideAnnotationBehavior <-
            if showAnnotation ^. ExprGui.showExpanded
            then pure KeepWideTypeAnnotation
            else isExprSelected <&> wideAnnotationBehaviorFromSelected
        maybeAddAnnotation wideAnnotationBehavior
            showAnnotation
            (pl ^. Sugar.plAnnotation)
            & Reader.local (Element.animIdPrefix .~ animId)
    where
        myId = WidgetIds.fromExprPayload pl
        isExprSelected =
            do
                isOnExpr <- GuiState.isSubCursor ?? myId
                isOnDotter <- GuiState.isSubCursor ?? WidgetIds.dotterId myId
                pure (isOnExpr && not isOnDotter)
        animId = WidgetIds.fromExprPayload pl & Widget.toAnimId
        showAnnotation = pl ^. Sugar.plData . ExprGui.plShowAnnotation

evaluationResult ::
    Monad m =>
    Sugar.Payload name (T m) ExprGui.Payload -> ExprGuiM m (Maybe (ER.Val Type))
evaluationResult pl =
    ExprGuiM.readMScopeId
    <&> valOfScope (pl ^. Sugar.plAnnotation)
    <&> Lens.mapped %~ erdVal

data EvalAnnotationOptions
    = NormalEvalAnnotation
    | WithNeighbouringEvalAnnotations (NeighborVals (Maybe Sugar.BinderParamScopeId))

data AnnotationMode
    = AnnotationModeNone
    | AnnotationModeTypes
    | AnnotationModeEvaluation (Maybe (NeighborVals (Maybe EvalResDisplay))) EvalResDisplay

getAnnotationMode :: Monad m => EvalAnnotationOptions -> Sugar.Annotation -> ExprGuiM m AnnotationMode
getAnnotationMode opt annotation =
    Lens.view (Settings.settings . Settings.sAnnotationMode)
    >>= \case
    AnnotationMode.None -> pure AnnotationModeNone
    AnnotationMode.Types -> pure AnnotationModeTypes
    AnnotationMode.Evaluation ->
        ExprGuiM.readMScopeId <&> valOfScope annotation
        <&> maybe AnnotationModeNone (AnnotationModeEvaluation neighbourVals)
    where
        neighbourVals =
            case opt of
            NormalEvalAnnotation -> Nothing
            WithNeighbouringEvalAnnotations neighbors ->
                neighbors <&> (>>= valOfScopePreferCur annotation . (^. Sugar.bParamScopeId))
                & Just

maybeAddAnnotationWith ::
    (Functor f, Monad m) =>
    EvalAnnotationOptions -> WideAnnotationBehavior -> ShowAnnotation ->
    Sugar.Annotation ->
    ExprGuiM m (Responsive (f GuiState.Update) -> Responsive (f GuiState.Update))
maybeAddAnnotationWith opt wideAnnotationBehavior showAnnotation annotation =
    getAnnotationMode opt annotation
    >>= \case
    AnnotationModeNone
        | _showExpanded showAnnotation -> withType
        | otherwise -> noAnnotation
    AnnotationModeEvaluation n v ->
        case _showInEvalMode showAnnotation of
        EvalModeShowNothing -> noAnnotation
        EvalModeShowType -> withType
        EvalModeShowEval -> withVal n v
    AnnotationModeTypes
        | _showInTypeMode showAnnotation -> withType
        | otherwise -> noAnnotation
    where
        noAnnotation = pure id
        -- concise mode and eval mode with no result
        inferredType = annotation ^. Sugar.aInferredType
        withType = addInferredType inferredType wideAnnotationBehavior
        withVal mNeighborVals scopeAndVal =
            do
                typeView <- TypeView.make inferredType <&> (^. Align.tValue)
                process <- processAnnotationGui wideAnnotationBehavior
                addEvaluationResult mNeighborVals scopeAndVal wideAnnotationBehavior
                    <&> \add -> add $ \width -> process width typeView ^. Element.width

maybeAddAnnotation ::
    (Functor f, Monad m) =>
    WideAnnotationBehavior -> ShowAnnotation -> Sugar.Annotation ->
    ExprGuiM m (Responsive (f GuiState.Update) -> Responsive (f GuiState.Update))
maybeAddAnnotation = maybeAddAnnotationWith NormalEvalAnnotation

valOfScope :: Sugar.Annotation -> CurAndPrev (Maybe ER.ScopeId) -> Maybe EvalResDisplay
valOfScope annotation mScopeIds =
    go
    <$> curPrevTag
    <*> annotation ^. Sugar.aMEvaluationResult
    <*> mScopeIds
    & fallbackToPrev
    where
        go _ _ Nothing = Nothing
        go tag ann (Just scopeId) =
            ann ^? Lens._Just . Lens.at scopeId . Lens._Just
            <&> EvalResDisplay scopeId tag

valOfScopePreferCur :: Sugar.Annotation -> ER.ScopeId -> Maybe EvalResDisplay
valOfScopePreferCur annotation = valOfScope annotation . pure . Just
