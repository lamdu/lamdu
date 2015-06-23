{-# LANGUAGE RecordWildCards, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui, egWidget, egAlignment
    -- General:
    , fromValueWidget, addBelow, addAbove
    , scaleFromTop, scale
    , pad
    , stdSpace
    , hbox, hboxSpaced
    , vboxTopFocal, vboxTopFocalSpaced
    , gridTopLeftFocal
    , listWithDelDests
    , makeLabel
    , grammarLabel
    , addValBG, addValFrame
    -- Lifted widgets:
    , makeFocusableView
    , makeNameView
    , makeNameEdit
    , makeNameOriginEdit
    , diveToNameEdit
    -- Info adding
    , annotationSpacer
    , maybeAddAnnotation
    , AnnotationOptions(..), maybeAddAnnotationWith
    , makeTypeView
    -- Expression wrapping
    , MyPrecedence(..), ParentPrecedence(..), Precedence
    , parenify
    , wrapExprEventMap
    , maybeAddAnnotationPl
    , stdWrap
    , stdWrapParentExpr
    , stdWrapParenify
    ) where

import           Control.Applicative ((<$>))
import           Control.Lens (Lens, Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Binary.Utils (encodeS)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Monoid (Monoid(..))
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Eval.Results (ComputedVal)
import           Lamdu.Eval.Val (ScopeId)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.EvalView as EvalView
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.Parens as Parens
import           Lamdu.GUI.Precedence (MyPrecedence(..), ParentPrecedence(..), Precedence)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

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
        ([(Widget.R, Widget (T m))] -> ExpressionGui m -> ExpressionGui m) ->
        Widget.R -> [(Widget.R, Widget (T m))] ->
        ExpressionGui m -> ExpressionGui m
alignAdd addFunc hAlign widgets eg =
    eg & egAlignment . _1 .~ hAlign & addFunc widgets

addAbove ::
        Widget.R -> [(Widget.R, Widget (T m))] ->
        ExpressionGui m -> ExpressionGui m
addAbove = alignAdd (Layout.addBefore Layout.Vertical)

addBelow ::
        Widget.R -> [(Widget.R, Widget (T m))] ->
        ExpressionGui m -> ExpressionGui m
addBelow = alignAdd (Layout.addAfter Layout.Vertical)

-- | Scale the given ExpressionGui without moving its top alignment
-- point:
scaleFromTop :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scaleFromTop = Layout.scaleFromTopLeft

scale :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scale = Layout.scale

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad = Layout.pad

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox = Layout.hbox 0.5

stdSpace :: MonadA m => ExprGuiM m (Layout.Layout f)
stdSpace =
    ExprGuiM.widgetEnv BWidgets.stdSpaceWidget
    <&> Layout.fromCenteredWidget

hboxSpaced :: MonadA m => [ExpressionGui f] -> ExprGuiM m (ExpressionGui f)
hboxSpaced guis =
    stdSpace
    <&> (`List.intersperse` guis)
    <&> hbox

vboxTopFocal :: [ExpressionGui m] -> ExpressionGui m
vboxTopFocal [] = Layout.empty
vboxTopFocal (gui:guis) = gui & Layout.addAfter Layout.Vertical guis

vboxTopFocalSpaced ::
    MonadA m => [ExpressionGui m] -> ExprGuiM m (ExpressionGui m)
vboxTopFocalSpaced guis =
    do
        space <- ExprGuiM.widgetEnv BWidgets.verticalSpace
        guis & List.intersperse (Layout.fromCenteredWidget space)
            & vboxTopFocal & return

gridTopLeftFocal :: [[ExpressionGui m]] -> ExpressionGui m
gridTopLeftFocal = Layout.gridTopLeftFocal

wWidth :: Lens' (Widget f) Widget.R
wWidth = Widget.width

addAnnotationBackground :: Config -> AnimId -> Widget.R -> View -> View
addAnnotationBackground config animId minWidth annotationView =
    annotationView
    & View.size .~ newSize
    & View.animFrame %~ Anim.translate (Vector2 ((width - annotationWidth) / 2) 0)
    & View.backgroundColor bgAnimId bgLayer bgColor
    where
        width = max annotationWidth minWidth
        annotationWidth = annotationView ^. View.width
        newSize = annotationView ^. View.size & _1 .~ width
        bgAnimId = animId ++ ["annotation background"]
        bgLayer = Config.layerAnnotations $ Config.layers config
        bgColor = Config.typeBoxBGColor config

makeWithAnnotationBG ::
    MonadA m => Widget.R -> Sugar.EntityId ->
    (AnimId -> ExprGuiM m View) -> ExprGuiM m (Widget f)
makeWithAnnotationBG minWidth entityId f =
    do
        config <- ExprGuiM.readConfig
        f animId
            <&> addAnnotationBackground config animId minWidth
            <&> Widget.fromView
    where
        animId = Widget.toAnimId $ WidgetIds.fromEntityId entityId

type ScopeAndVal = (ScopeId, ComputedVal ())

makeEvaluationResultView :: MonadA m => ScopeAndVal -> AnimId -> ExprGuiM m View
makeEvaluationResultView (scopeId, evalRes) animId =
    EvalView.make (animId ++ [encodeS scopeId]) evalRes

makeTypeView ::
        MonadA m => Sugar.EntityId -> Type -> Widget.R -> ExprGuiM m (Widget f)
makeTypeView entityId typ minWidth =
    makeWithAnnotationBG minWidth entityId (`TypeView.make` typ)

makeEvalView ::
    MonadA m =>
    Sugar.EntityId -> (Maybe ScopeAndVal, Maybe ScopeAndVal) -> ScopeAndVal ->
    Widget.R -> ExprGuiM m (Widget f)
makeEvalView entityId (mPrev, mNext) evalRes minWidth =
    makeWithAnnotationBG minWidth entityId $
    \animId ->
        do
            Config.Eval{..} <- ExprGuiM.readConfig <&> Config.eval
            let neighbourViews n =
                    n ^.. Lens._Just
                    <&> (`makeEvaluationResultView` animId)
                    <&> Lens.mapped %~ View.scale (neighborsScaleFactor <&> realToFrac)
            neighbourViews mPrev
                ++ makeEvaluationResultView evalRes animId
                : neighbourViews mNext
                & sequence
                <&> GridView.horizontalAlign 0.5

annotationSpacer :: MonadA m => ExprGuiM m (Widget f)
annotationSpacer =
    do
        config <- ExprGuiM.readConfig
        Config.valAnnotationSpacing config
            & realToFrac
            & Spacer.makeVertical
            & Widget.fromView
            & return

addAnnotationH ::
    MonadA m =>
    (Widget.R -> ExprGuiM m (Widget (T m))) ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addAnnotationH f eg =
    do
        vspace <- annotationSpacer
        widget <- f (eg ^. egWidget . wWidth)
        addBelow 0.5 [(0, vspace), (0.5, widget)] eg
            & return

addInferredType ::
    MonadA m => Sugar.EntityId -> Type ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addInferredType entityId = addAnnotationH . makeTypeView entityId

addEvaluationResult ::
    MonadA m =>
    Sugar.EntityId -> (Maybe ScopeAndVal, Maybe ScopeAndVal) -> ScopeAndVal ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addEvaluationResult entityId prevNext =
    addAnnotationH . makeEvalView entityId prevNext

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
makeNameEdit (Name nameSrc nameCollision setName name) myId =
    do
        collisionSuffixes <-
            makeCollisionSuffixLabels nameCollision (Widget.toAnimId myId)
        nameEdit <-
            makeWordEdit (Property storedName setName) (diveToNameEdit myId)
            & WE.localEnv emptyStringEnv
            & ExprGuiM.widgetEnv
        return . Box.hboxCentered $ nameEdit : collisionSuffixes
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
    ParentPrecedence -> MyPrecedence -> Widget.Id ->
    ExpressionGui f -> ExprGuiM m (ExpressionGui f)
parenify (ParentPrecedence parent) (MyPrecedence prec) myId widget
    | parent > prec =
        widget & Parens.addHighlightedTextParens myId
    | otherwise =
        widget & return

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
addValBG myId gui =
    do
        config <- ExprGuiM.readConfig
        let layer = Config.layerValFrameBG $ Config.layers config
        let color = Config.valFrameBGColor config
        Widget.backgroundColor layer animId color gui & return
    where
        animId = Widget.toAnimId myId ++ ["val"]

addValFrame ::
    MonadA m => Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addValFrame myId gui =
    do
        config <- ExprGuiM.readConfig
        gui
            & pad (realToFrac <$> Config.valFramePadding config)
            & egWidget %%~ addValBG myId

stdWrapParenify ::
    MonadA m =>
    Sugar.Payload m ExprGuiT.Payload -> ParentPrecedence -> MyPrecedence ->
    (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParenify pl parentPrec prec mkGui =
    stdWrapParentExpr pl $ \myId ->
    mkGui myId
    >>= parenify parentPrec prec myId

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
maybeAddAnnotationPl pl =
    maybeAddAnnotation
    (pl ^. Sugar.plData . ExprGuiT.plShowAnnotation)
    (pl ^. Sugar.plAnnotation)
    (pl ^. Sugar.plEntityId)

data MissingAnnotationBehavior = ShowNothing | ShowType

data AnnotationOptions
    = NormalAnnotation
    | WithNeighbouringAnnotations (Maybe ScopeId) (Maybe ScopeId)

maybeAddAnnotationH ::
    MonadA m =>
    AnnotationOptions -> MissingAnnotationBehavior ->
    Sugar.Annotation -> Sugar.EntityId -> ExpressionGui m ->
    ExprGuiM m (ExpressionGui m)
maybeAddAnnotationH opt missingAnnotationBehavior annotation entityId eg =
    do
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.None -> handleMissingAnnotation
            CESettings.Types -> withType
            CESettings.Evaluation ->
                ExprGuiM.readMScopeId <&> (>>= valAndScope)
                >>= maybe handleMissingAnnotation
                    (($ eg) . addEvaluationResult entityId neighbourVals)
                where
                    neighbourVals =
                        case opt of
                        NormalAnnotation -> (Nothing, Nothing)
                        WithNeighbouringAnnotations p n ->
                            (p >>= valAndScope, n >>= valAndScope)
    where
        handleMissingAnnotation =
            case missingAnnotationBehavior of
            ShowNothing -> return eg
            ShowType -> withType
        withType = addInferredType entityId (annotation ^. Sugar.aInferredType) eg
        valAndScope scopeId = valOfScope scopeId <&> (,) scopeId
        valOfScope scopeId =
            annotation ^? Sugar.aMEvaluationResult .
            Lens._Just . Lens.at scopeId . Lens._Just

maybeAddAnnotation ::
    MonadA m =>
    ExprGuiT.ShowAnnotation -> Sugar.Annotation -> Sugar.EntityId ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
maybeAddAnnotation = maybeAddAnnotationWith NormalAnnotation

maybeAddAnnotationWith ::
    MonadA m =>
    AnnotationOptions ->
    ExprGuiT.ShowAnnotation -> Sugar.Annotation -> Sugar.EntityId ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
maybeAddAnnotationWith _ ExprGuiT.DoNotShowAnnotation _ _ eg = return eg
maybeAddAnnotationWith o ExprGuiT.ShowAnnotation annotation entityId eg =
    maybeAddAnnotationH o ShowType annotation entityId eg
maybeAddAnnotationWith o ExprGuiT.ShowAnnotationInVerboseMode annotation entityId eg =
    maybeAddAnnotationH o ShowNothing annotation entityId eg

listWithDelDests :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
listWithDelDests before after dest list =
    ListUtils.withPrevNext
    (maybe before dest (list ^? Lens.ix 1))
    (maybe after dest (reverse list ^? Lens.ix 1))
    dest list
