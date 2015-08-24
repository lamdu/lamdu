{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, RankNTypes, TypeFamilies, LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui, egWidget, egAlignment
    -- General:
    , fromValueWidget, addBelow, addAbove
    , scale
    , pad
    , stdSpace, verticalSpace
    , hbox, hboxSpaced
    , vboxTopFocal, vboxTopFocalSpaced, vboxTopFocalAlignedTo
    , gridTopLeftFocal
    , listWithDelDests
    , makeLabel
    , grammarLabel
    , addValBG, addValFrame, addValPadding
    , addValBGWithColor
    , liftLayers
    -- Lifted widgets:
    , makeFocusableView
    , makeNameView
    , makeNameEdit, makeNameEditWith
    , makeNameOriginEdit
    , diveToNameEdit
    -- Info adding
    , annotationSpacer
    , NeighborVals(..)
    , EvalAnnotationOptions(..), maybeAddAnnotationWith
    , WideAnnotationBehavior(..), wideAnnotationBehaviorFromSelected
    , AnnotationParams(..), annotationParamsFor
    , makeTypeView
    , evaluationResult
    -- Expression wrapping
    , MyPrecedence(..), ParentPrecedence(..), Precedence(..), Precedence.precLeft, Precedence.precRight
    , parenify
    , wrapExprEventMap
    , maybeAddAnnotationPl
    , stdWrap
    , stdWrapParentExpr
    , stdWrapParenify
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens (Lens, Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Binary.Utils (encodeS)
import           Data.CurAndPrev (current, prev)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Eval.Val (EvalResult, ScopeId)
import qualified Lamdu.Eval.Val as EV
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.EvalView as EvalView
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui, ShowAnnotation(..), EvalModeShow(..))
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Parens as Parens
import           Lamdu.GUI.Precedence (MyPrecedence(..), ParentPrecedence(..), Precedence(..), needParens)
import qualified Lamdu.GUI.Precedence as Precedence
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

{-# INLINE egWidget #-}
egWidget ::
    Lens (ExpressionGui m) (ExpressionGui n) (Widget (T m)) (Widget (T n))
egWidget = Layout.alignedWidget . _2

{-# INLINE egAlignment #-}
egAlignment :: Lens' (ExpressionGui m) Layout.Alignment
egAlignment = Layout.alignedWidget . _1

fromValueWidget :: Widget (T m) -> ExpressionGui m
fromValueWidget = Layout.fromCenteredWidget

alignAdd ::
    (t -> ExpressionGui m -> b) -> Widget.R ->
    t -> ExpressionGui m -> b
alignAdd addFunc hAlign widgets eg =
    eg & egAlignment . _1 .~ hAlign & addFunc widgets

addAbove ::
    (Layout.AddLayout w, Layout.LayoutType w ~ ExpressionGui m) =>
    Widget.R -> [w] -> ExpressionGui m -> ExpressionGui m
addAbove = alignAdd (Layout.addBefore Layout.Vertical)

addBelow ::
    (Layout.AddLayout w, Layout.LayoutType w ~ ExpressionGui m) =>
    Widget.R -> [w] -> ExpressionGui m -> ExpressionGui m
addBelow = alignAdd (Layout.addAfter Layout.Vertical)

scale :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scale = Layout.scale

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad = Layout.pad

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox = Layout.hbox 0.5

stdSpace :: MonadA m => ExprGuiM m (Layout.Layout f)
stdSpace = ExprGuiM.widgetEnv BWidgets.stdSpaceWidget <&> Layout.fromCenteredWidget

verticalSpace :: MonadA m => ExprGuiM m (ExpressionGui f)
verticalSpace = ExprGuiM.widgetEnv BWidgets.verticalSpace <&> Layout.fromCenteredWidget

hboxSpaced :: MonadA m => [ExpressionGui f] -> ExprGuiM m (ExpressionGui f)
hboxSpaced guis =
    stdSpace
    <&> (`List.intersperse` guis)
    <&> hbox

vboxTopFocalAlignedTo :: Widget.R -> [ExpressionGui m] -> ExpressionGui m
vboxTopFocalAlignedTo hAlign guis =
    guis <&> egAlignment . _1 .~ hAlign & vboxTopFocal

vboxTopFocal :: [ExpressionGui m] -> ExpressionGui m
vboxTopFocal [] = Layout.empty
vboxTopFocal (gui:guis) = gui & Layout.addAfter Layout.Vertical guis

vboxTopFocalSpaced ::
    MonadA m => [ExpressionGui m] -> ExprGuiM m (ExpressionGui m)
vboxTopFocalSpaced guis =
    do
        space <- verticalSpace
        guis & List.intersperse space & vboxTopFocal & return

gridTopLeftFocal :: [[ExpressionGui m]] -> ExpressionGui m
gridTopLeftFocal = Layout.gridTopLeftFocal

addAnnotationBackgroundH :: (Config -> Draw.Color) -> Config -> AnimId -> ExpressionGui m -> ExpressionGui m
addAnnotationBackgroundH getColor config animId =
    egWidget %~ Widget.backgroundColor bgLayer bgAnimId bgColor
    where
        bgAnimId = animId ++ ["annotation background"]
        bgLayer = Config.layerAnnotations $ Config.layers config
        bgColor = getColor config

addAnnotationBackground :: Config -> AnimId -> ExpressionGui m -> ExpressionGui m
addAnnotationBackground = addAnnotationBackgroundH Config.valAnnotationBGColor

addAnnotationHoverBackground :: Config -> AnimId -> ExpressionGui m -> ExpressionGui m
addAnnotationHoverBackground = addAnnotationBackgroundH Config.valAnnotationHoverBGColor

data WideAnnotationBehavior
    = ShrinkWideAnnotation
    | HoverWideAnnotation
    | KeepWideAnnotation

wideAnnotationBehaviorFromSelected :: Bool -> WideAnnotationBehavior
wideAnnotationBehaviorFromSelected False = ShrinkWideAnnotation
wideAnnotationBehaviorFromSelected True = HoverWideAnnotation

data AnnotationParams = AnnotationParams
    { apMinWidth :: Widget.R
    , apAnimId :: AnimId
    , apWideAnnotationBehavior :: WideAnnotationBehavior
    }

annotationParamsFor ::
    WideAnnotationBehavior -> Sugar.EntityId -> ExpressionGui m -> AnnotationParams
annotationParamsFor wideBehavior entityId eg =
    AnnotationParams
    { apMinWidth = eg ^. egWidget . Widget.width
    , apAnimId = Widget.toAnimId $ WidgetIds.fromEntityId entityId
    , apWideAnnotationBehavior = wideBehavior
    }

-- NOTE: Also adds the background color, because it differs based on
-- whether we're hovering
applyWideAnnotationBehavior ::
    MonadA m => Config -> AnimId -> WideAnnotationBehavior -> Vector2 Widget.R -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
applyWideAnnotationBehavior config animId wideAnnotationBehavior shrinkRatio eg =
    case wideAnnotationBehavior of
    ShrinkWideAnnotation ->
        scaledDown
        & addAnnotationBackground config animId
        & return
    HoverWideAnnotation ->
        eg
        & addAnnotationHoverBackground config animId
        & (`Layout.hoverInPlaceOf` scaledDown)
        & liftLayers
    KeepWideAnnotation -> return eg
    where
        scaledDown =
            eg
            & Layout.scaleAround (Vector2 0.5 0) shrinkRatio

makeWithAnnotationBG ::
    MonadA m => (AnimId -> ExprGuiM m (ExpressionGui m)) ->
    AnnotationParams -> ExprGuiM m (ExpressionGui m)
makeWithAnnotationBG f (AnnotationParams minWidth animId wideAnnotationBehavior) =
    do
        config <- ExprGuiM.readConfig
        annotationEg <- f animId
        let annotationWidth = annotationEg ^. egWidget . Widget.width
        let width = max annotationWidth minWidth
        let expansionLimit =
                Config.valAnnotationWidthExpansionLimit config & realToFrac
        let maxWidth = minWidth + expansionLimit
        let shrinkAtLeast = Config.valAnnotationShrinkAtLeast config & realToFrac
        let shrinkRatio =
                annotationWidth - shrinkAtLeast & min maxWidth & max minWidth
                & (/ annotationWidth) & pure
        let maybeTooNarrow
                | minWidth > annotationWidth = pad (Vector2 ((width - annotationWidth) / 2) 0)
                | otherwise = id
        let maybeTooWide
                | annotationWidth > minWidth + max shrinkAtLeast expansionLimit =
                    applyWideAnnotationBehavior config animId
                    wideAnnotationBehavior shrinkRatio
                | otherwise = return . addAnnotationBackground config animId
        annotationEg
            & maybeTooNarrow
            & maybeTooWide

data ResultSource = Current | Previous

data EvalResDisplay = EvalResDisplay
    { erdScope :: ScopeId
    , erdSource :: ResultSource
    , erdVal :: EvalResult ()
    }

makeEvaluationResultView ::
    MonadA m => AnimId -> EvalResDisplay -> ExprGuiM m (ExpressionGui m)
makeEvaluationResultView animId res =
    EvalView.make (animId ++ [encodeS (erdScope res)]) (erdVal res)
    >>= case erdSource res of
        Current -> return
        Previous ->
            \view ->
            do
                config <- ExprGuiM.readConfig
                View.tint (Config.staleResultTint (Config.eval config)) view
                    & return
    <&> Widget.fromView
    <&> fromValueWidget

makeTypeView :: MonadA m => Type -> AnimId -> ExprGuiM m (ExpressionGui m)
makeTypeView typ animId =
    TypeView.make animId typ <&> fromValueWidget . Widget.fromView

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable)

makeEvalView ::
    MonadA m =>
    NeighborVals (Maybe EvalResDisplay) -> EvalResDisplay ->
    AnimId -> ExprGuiM m (ExpressionGui m)
makeEvalView (NeighborVals mPrev mNext) evalRes animId =
    do
        config <- ExprGuiM.readConfig
        let Config.Eval{..} = Config.eval config
        let makeEvaluationResultViewBG res =
                makeEvaluationResultView animId res
                <&> addAnnotationBackground config (animId ++ [encodeS (erdScope res)])
        let neighbourViews n =
                n ^.. Lens._Just
                <&> makeEvaluationResultViewBG
                <&> Lens.mapped %~
                    pad (neighborsPadding <&> realToFrac) .
                    scale (neighborsScaleFactor <&> realToFrac)
        prevs <- sequence (neighbourViews mPrev)
        nexts <- sequence (neighbourViews mNext)
        evalView <- makeEvaluationResultView animId evalRes
        evalView
            & Layout.addBefore Layout.Horizontal prevs
            & Layout.addAfter Layout.Horizontal nexts
            & (`Layout.hoverInPlaceOf` evalView)
            & return

annotationSpacer :: MonadA m => ExprGuiM m (ExpressionGui f)
annotationSpacer =
    do
        config <- ExprGuiM.readConfig
        Config.valAnnotationSpacing config
            & realToFrac
            & Spacer.makeVertical
            & Widget.fromView
            & fromValueWidget
            & return

addAnnotationH ::
    MonadA m =>
    (AnimId -> ExprGuiM m (ExpressionGui m)) ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addAnnotationH f wideBehavior entityId eg =
    do
        vspace <- annotationSpacer
        annotationEg <-
            makeWithAnnotationBG f (annotationParamsFor wideBehavior entityId eg)
        vboxTopFocal
            [ eg & egAlignment . _1 .~ 0.5
            , vspace
            , annotationEg
            ] & return

addInferredType ::
    MonadA m => Type -> WideAnnotationBehavior -> Sugar.EntityId ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addInferredType typ = addAnnotationH (makeTypeView typ)

addEvaluationResult ::
    MonadA m =>
    Type -> NeighborVals (Maybe EvalResDisplay) -> EvalResDisplay ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addEvaluationResult _typ _neigh EvalResDisplay{erdVal = Right EV.HRecEmpty} _wide entityId gui =
    gui
    & egWidget %%~
        addValBGWithColor Config.evaluatedPathBGColor
        (WidgetIds.fromEntityId entityId)
addEvaluationResult typ _neigh EvalResDisplay{erdVal = Right EV.HFunc{}} wideBehavior entityId gui =
    addAnnotationH (makeTypeView typ) wideBehavior entityId gui
addEvaluationResult _typ prevNext res wideBehavior entityId gui =
    addAnnotationH (makeEvalView prevNext res) wideBehavior entityId gui

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = Config.enterSubexpressionKeys config
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Enter subexpression"]
    , FocusDelegator.focusParentKeys = Config.leaveSubexpressionKeys config
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Leave subexpression"]
    }

disallowedNameChars :: String
disallowedNameChars = "[]\\`()"

nameEditFDConfig :: FocusDelegator.Config
nameEditFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Rename"]
    , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Done renaming"]
    }

makeNameOriginEdit :: MonadA m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m))
makeNameOriginEdit name myId =
    do
        config <- Config.name <$> ExprGuiM.readConfig
        makeNameEdit name myId -- myId goes directly to name edit
            & ExprGuiM.withFgColor (color config)
    where
        color =
            case nNameSource name of
            NameSourceAutoGenerated -> Config.autoNameOriginFGColor
            NameSourceStored -> Config.nameOriginFGColor

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = WidgetIds.delegatingId

makeNameEdit :: MonadA m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m))
makeNameEdit = makeNameEditWith id

makeNameEditWith ::
    MonadA m => (Widget (T m) -> Widget (T m)) -> Name m -> Widget.Id -> ExprGuiM m (Widget (T m))
makeNameEditWith onActiveEditor (Name nameSrc nameCollision setName name) myId =
    do
        collisionSuffixes <-
            makeCollisionSuffixLabels nameCollision (Widget.toAnimId myId)
        nameEdit <-
            makeWordEdit (Property storedName setName) (diveToNameEdit myId)
            & WE.localEnv emptyStringEnv
            & ExprGuiM.widgetEnv
        return . Box.hboxCentered $ nameEdit : collisionSuffixes
    <&> onActiveEditor
    >>= ExprGuiM.makeFocusDelegator nameEditFDConfig
        FocusDelegator.FocusEntryParent myId
    where
        emptyStringEnv env = env
            & WE.envTextStyle . TextEdit.sEmptyFocusedString .~ ""
            & WE.envTextStyle . TextEdit.sEmptyUnfocusedString .~ name
        storedName =
            case nameSrc of
            NameSourceAutoGenerated -> ""
            NameSourceStored -> name
        makeWordEdit =
            BWidgets.makeWordEdit <&>
            Lens.mapped . Lens.mapped . Widget.eventMap %~
            E.filterChars (`notElem` disallowedNameChars)

stdWrap ::
    MonadA m => Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
stdWrap pl mkGui =
    mkGui
    >>= maybeAddAnnotationPl pl
    & wrapExprEventMap pl

stdWrapParentExpr ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload ->
    (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl mkGui = do
    config <- ExprGuiM.readConfig
    mkGui innerId
        >>= egWidget %%~
                ExprGuiM.makeFocusDelegator (parentExprFDConfig config)
                FocusDelegator.FocusEntryChild outerId
        & stdWrap pl
        & ExprGuiM.assignCursor myId innerId
    where
        myId = WidgetIds.fromExprPayload pl
        outerId = WidgetIds.notDelegatingId myId
        innerId = WidgetIds.delegatingId myId

makeFocusableView ::
    (MonadA m, MonadA n) =>
    Widget.Id -> ExpressionGui n -> ExprGuiM m (ExpressionGui n)
makeFocusableView myId gui =
    ExprGuiM.widgetEnv $
    egWidget (BWidgets.makeFocusableView myId) gui

parenify ::
    (MonadA f, MonadA m) =>
    MyPrecedence -> Widget.Id ->
    ExprGuiM m (ExpressionGui f) -> ExprGuiM m (ExpressionGui f)
parenify prec myId mkGui =
    do
        parent <- ExprGuiM.outerPrecedence
        if needParens (ParentPrecedence parent) prec
              then mkGui >>= Parens.addHighlightedTextParens myId
                   & ExprGuiM.withLocalPrecedence (const 0)
              else mkGui

makeLabel :: MonadA m => String -> AnimId -> ExprGuiM m (ExpressionGui m)
makeLabel text animId = ExprGuiM.makeLabel text animId <&> fromValueWidget

grammarLabel :: MonadA m => String -> AnimId -> ExprGuiM m (ExpressionGui m)
grammarLabel text animId =
    do
        config <- ExprGuiM.readConfig
        makeLabel text animId
            & ExprGuiM.localEnv
                (WE.setTextSizeColor (Config.baseTextSize config) (Config.grammarColor config))

addValBG :: MonadA m => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
addValBG = addValBGWithColor Config.valFrameBGColor

addValBGWithColor ::
    MonadA m =>
    (Config -> Draw.Color) -> Widget.Id -> Widget f -> ExprGuiM m (Widget f)
addValBGWithColor color myId gui =
    do
        config <- ExprGuiM.readConfig
        let layer = Config.layerValFrameBG $ Config.layers config
        Widget.backgroundColor layer animId (color config) gui & return
    where
        animId = Widget.toAnimId myId ++ ["val"]

addValPadding :: MonadA m => ExpressionGui n -> ExprGuiM m (ExpressionGui n)
addValPadding gui =
    do
        padding <- ExprGuiM.readConfig <&> Config.valFramePadding
        pad (padding <&> realToFrac) gui & return

liftLayers :: MonadA m => ExpressionGui n -> ExprGuiM m (ExpressionGui n)
liftLayers =
    egWidget %%~ ExprGuiM.widgetEnv . BWidgets.liftLayerInterval

addValFrame ::
    MonadA m => Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addValFrame myId gui = addValPadding gui >>= egWidget %%~ addValBG myId

stdWrapParenify ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload -> MyPrecedence ->
    (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParenify pl prec mkGui =
    stdWrapParentExpr pl $ \myId ->
    mkGui myId
    & parenify prec myId

-- TODO: This doesn't belong here
makeNameView ::
    (MonadA m, MonadA n) =>
    Name n -> AnimId -> ExprGuiM m (Widget f)
makeNameView (Name _ collision _ name) animId =
    do
        label <- BWidgets.makeLabel name animId & ExprGuiM.widgetEnv
        suffixLabels <- makeCollisionSuffixLabels collision $ animId ++ ["suffix"]
        Box.hboxCentered (label : suffixLabels) & return

-- TODO: This doesn't belong here
makeCollisionSuffixLabels ::
    MonadA m => NameCollision -> AnimId -> ExprGuiM m [Widget f]
makeCollisionSuffixLabels NoCollision _ = return []
makeCollisionSuffixLabels (Collision suffix) animId =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
            onSuffixWidget =
                Widget.backgroundColor (Config.layerNameCollisionBG (Config.layers config))
                    animId collisionSuffixBGColor .
                Widget.scale (realToFrac <$> collisionSuffixScaleFactor)
        BWidgets.makeLabel (show suffix) animId
            & WE.localEnv (WE.setTextColor collisionSuffixTextColor)
            <&> (:[]) . onSuffixWidget
            & ExprGuiM.widgetEnv

wrapExprEventMap ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
wrapExprEventMap pl action =
    do
        (res, resultPickers) <- ExprGuiM.listenResultPickers action
        res & addExprEventMap pl resultPickers

addExprEventMap ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload -> HolePickers m ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addExprEventMap pl resultPickers gui =
    do
        exprEventMap <- ExprEventMap.make resultPickers pl
        gui
            & egWidget %~ Widget.weakerEvents exprEventMap
            & return

maybeAddAnnotationPl ::
    MonadA m =>
    Sugar.Payload m0 ExprGuiT.Payload ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
maybeAddAnnotationPl pl eg =
    do
        wideAnnotationBehavior <-
            ExprGuiM.isExprSelected pl <&> wideAnnotationBehaviorFromSelected
        eg
            & maybeAddAnnotation wideAnnotationBehavior
              (pl ^. Sugar.plData . ExprGuiT.plShowAnnotation)
              (pl ^. Sugar.plAnnotation)
              (pl ^. Sugar.plEntityId)

evaluationResult ::
    MonadA m => Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (Maybe (EvalResult ()))
evaluationResult pl =
    ExprGuiM.readMScopeId
    <&> (>>= valOfScope (pl ^. Sugar.plAnnotation))
    <&> Lens.mapped %~ erdVal

data EvalAnnotationOptions
    = NormalEvalAnnotation
    | WithNeighbouringEvalAnnotations (NeighborVals (Maybe Sugar.BinderParamScopeId))

maybeAddAnnotation ::
    MonadA m =>
    WideAnnotationBehavior -> ShowAnnotation -> Sugar.Annotation -> Sugar.EntityId ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
maybeAddAnnotation = maybeAddAnnotationWith NormalEvalAnnotation

data AnnotationMode
    = AnnotationModeNone
    | AnnotationModeTypes
    | AnnotationModeEvaluation (NeighborVals (Maybe EvalResDisplay)) EvalResDisplay

getAnnotationMode :: MonadA m => EvalAnnotationOptions -> Sugar.Annotation -> ExprGuiM m AnnotationMode
getAnnotationMode opt annotation =
    do
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.None -> return AnnotationModeNone
            CESettings.Types -> return AnnotationModeTypes
            CESettings.Evaluation ->
                ExprGuiM.readMScopeId <&> (>>= valOfScope annotation)
                <&> maybe AnnotationModeNone (AnnotationModeEvaluation neighbourVals)
    where
        neighbourVals =
            case opt of
            NormalEvalAnnotation -> NeighborVals Nothing Nothing
            WithNeighbouringEvalAnnotations neighbors ->
                neighbors <&> (>>= valOfScope annotation . (^. Sugar.bParamScopeId))

maybeAddAnnotationWith ::
    MonadA m =>
    EvalAnnotationOptions -> WideAnnotationBehavior -> ShowAnnotation ->
    Sugar.Annotation -> Sugar.EntityId -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
maybeAddAnnotationWith opt wideAnnotationBehavior ShowAnnotation{..} annotation entityId eg =
    getAnnotationMode opt annotation
    >>= \case
        AnnotationModeNone
            | _showTypeWhenMissing -> withType
            | otherwise -> noAnnotation
        AnnotationModeEvaluation n v ->
            case _showInEvalMode of
            EvalModeShowNothing -> noAnnotation
            EvalModeShowType -> withType
            EvalModeShowEval -> withVal n v
        AnnotationModeTypes
            | _showInTypeMode -> withType
            | otherwise -> noAnnotation
    where
        noAnnotation = return eg
        -- concise mode and eval mode with no result
        inferredType = annotation ^. Sugar.aInferredType
        withType = addInferredType inferredType wideAnnotationBehavior entityId eg
        withVal neighborVals scopeAndVal =
            addEvaluationResult inferredType neighborVals scopeAndVal
            wideAnnotationBehavior entityId eg

valOfScope :: Sugar.Annotation -> ScopeId -> Maybe EvalResDisplay
valOfScope annotation scopeId =
    go current Current <|> go prev Previous
    where
        go l t =
            annotation ^? Sugar.aMEvaluationResult . l .
            Lens._Just . Lens.at scopeId . Lens._Just
            <&> EvalResDisplay scopeId t

listWithDelDests :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
listWithDelDests = ListUtils.withPrevNext
