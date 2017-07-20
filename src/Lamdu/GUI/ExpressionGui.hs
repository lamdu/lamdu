{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, RankNTypes, TypeFamilies, LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable , FlexibleContexts #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui
    , render
    -- General:
    , combine, combineSpaced, combineSpacedMParens
    , horizVertFallback
    , listWithDelDests
    , grammarLabel
    , addValFrame, addValPadding
    , addValBGWithColor
    -- Lifted widgets:
    , makeNameView
    , makeNameEdit
    , makeNameOriginEdit, styleNameOrigin
    , addDeletionDiagonal
    -- Info adding
    , annotationSpacer
    , NeighborVals(..)
    , EvalAnnotationOptions(..), maybeAddAnnotationWith
    , WideAnnotationBehavior(..), wideAnnotationBehaviorFromSelected
    , evaluationResult
    -- Expression wrapping

    , Precedence.MyPrecedence(..)
    , Precedence.ParentPrecedence(..)
    , Precedence.Precedence(..)
    , Precedence.before, Precedence.after

    , maybeAddAnnotationPl
    , stdWrap
    , parentDelegator
    , stdWrapParentExpr
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Binary.Utils (encodeS)
import           Data.CurAndPrev (CurAndPrev(..), CurPrevTag(..), curPrevTag, fallbackToPrev)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.View (View, (/|/), (/-/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Align (Aligned(..), WithTextPos(..))
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout(..))
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextEdit.Property as TextEdits
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.EvalView as EvalView
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui, ShowAnnotation(..), EvalModeShow(..))
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Precedence as Precedence
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

maybeIndent ::
    Functor f =>
    Maybe ParenIndentInfo ->
    TreeLayout (f Widget.EventResult) ->
    TreeLayout (f Widget.EventResult)
maybeIndent Nothing = id
maybeIndent (Just piInfo) =
    TreeLayout.render %~ f
    where
        f mkLayout lp =
            case lp ^. TreeLayout.layoutContext of
            TreeLayout.LayoutVertical ->
                indentBar /|/ Spacer.make (Vector2 gapWidth 0) /|/ content
                where
                    indentBar =
                        Spacer.make (Vector2 barWidth (content ^. View.height))
                        & View.backgroundColor bgAnimId (Theme.indentBarColor indentConf)
                    indentConf = piIndentTheme piInfo
                    stdSpace = piStdHorizSpacing piInfo
                    barWidth = stdSpace * Theme.indentBarWidth indentConf
                    gapWidth = stdSpace * Theme.indentBarGap indentConf
                    indentWidth = barWidth + gapWidth
                    content =
                        lp & TreeLayout.layoutMode . TreeLayout.modeWidths -~ indentWidth
                        & mkLayout
                    bgAnimId = piAnimId piInfo ++ ["("]
            _ -> mkLayout lp

data ParenIndentInfo = ParenIndentInfo
    { piAnimId :: AnimId
    , piTextStyle :: TextView.Style
    , piIndentTheme :: Theme.Indent
    , piStdHorizSpacing :: Widget.R
    }

parenLabel :: ParenIndentInfo -> Text -> WithTextPos View
parenLabel parenInfo t =
    TextView.make (piTextStyle parenInfo) t
    (piAnimId parenInfo ++ [encodeUtf8 t])

horizVertFallback ::
    (Monad m, Functor f) =>
    Maybe AnimId ->
    ExprGuiM m
    (TreeLayout (f Widget.EventResult) ->
     TreeLayout (f Widget.EventResult) ->
     TreeLayout (f Widget.EventResult))
horizVertFallback mParenId =
    mParenId & Lens._Just %%~ makeParenIndentInfo
    <&> horizVertFallbackH

horizVertFallbackH ::
    Functor f =>
    Maybe ParenIndentInfo ->
    TreeLayout (f Widget.EventResult) ->
    TreeLayout (f Widget.EventResult) ->
    TreeLayout (f Widget.EventResult)
horizVertFallbackH mParenInfo horiz vert =
    TreeLayout.render #
    \layoutParams ->
    let wide =
            layoutParams & TreeLayout.layoutMode .~ TreeLayout.LayoutWide
            & horiz ^. TreeLayout.render
    in
    case layoutParams ^. TreeLayout.layoutMode of
    TreeLayout.LayoutWide ->
        case (mParenInfo, layoutParams ^. TreeLayout.layoutContext) of
        (Just parenInfo, TreeLayout.LayoutHorizontal) ->
            parenLabel parenInfo "("
            /|/ wide /|/
            parenLabel parenInfo ")"
        _ -> wide
    TreeLayout.LayoutNarrow limit
        | wide ^. View.width > limit ->
            layoutParams & maybeIndent mParenInfo vert ^. TreeLayout.render
        | otherwise -> wide

combineWith ::
    Functor f => Maybe ParenIndentInfo ->
    ([WithTextPos (Widget (f Widget.EventResult))] ->
     [WithTextPos (Widget (f Widget.EventResult))]) ->
    ([TreeLayout (f Widget.EventResult)] ->
     [TreeLayout (f Widget.EventResult)]) ->
    [TreeLayout (f Widget.EventResult)] -> TreeLayout (f Widget.EventResult)
combineWith mParenInfo onHGuis onVGuis guis =
    horizVertFallbackH mParenInfo wide vert
    where
        vert = TreeLayout.vbox (onVGuis guis)
        wide =
            guis ^.. Lens.traverse . TreeLayout.render
            ?? TreeLayout.LayoutParams
                { _layoutMode = TreeLayout.LayoutWide
                , _layoutContext = TreeLayout.LayoutHorizontal
                }
            & onHGuis
            & View.hbox
            & TreeLayout.fromWithTextPos

combine ::
    Functor f => [TreeLayout (f Widget.EventResult)] ->
    TreeLayout (f Widget.EventResult)
combine = combineWith Nothing id id

makeParenIndentInfo ::
    (MonadReader env m, TextView.HasStyle env, Theme.HasTheme env, Spacer.HasStdSpacing env) =>
    AnimId -> m ParenIndentInfo
makeParenIndentInfo parensId =
    do
        textStyle <- Lens.view TextView.style
        theme <- Lens.view Theme.theme <&> Theme.indent
        stdSpacing <- Spacer.getSpaceSize <&> (^. _1)
        ParenIndentInfo parensId textStyle theme stdSpacing & return

combineSpaced ::
    (MonadReader env m, TextView.HasStyle env, Theme.HasTheme env,
     Spacer.HasStdSpacing env, Functor f) =>
    m ([TreeLayout (f Widget.EventResult)] -> TreeLayout (f Widget.EventResult))
combineSpaced = combineSpacedMParens Nothing

combineSpacedMParens ::
    (MonadReader env m, TextView.HasStyle env, Theme.HasTheme env,
     Spacer.HasStdSpacing env, Functor f) =>
    Maybe AnimId ->
    m ([TreeLayout (f Widget.EventResult)] ->
       TreeLayout (f Widget.EventResult))
combineSpacedMParens mParensId =
    do
        hSpace <- Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0
        vSpace <- Spacer.stdVSpace <&> TreeLayout.fromView
        mParenInfo <- mParensId & Lens._Just %%~ makeParenIndentInfo
        return $ combineWith mParenInfo (List.intersperse hSpace) (List.intersperse vSpace)

addAnnotationBackgroundH ::
    View.SetLayers a =>
    (Theme.ValAnnotation -> Draw.Color) -> Theme.ValAnnotation -> AnimId -> a -> a
addAnnotationBackgroundH getColor theme animId =
    View.backgroundColor bgAnimId bgColor
    where
        bgAnimId = animId ++ ["annotation background"]
        bgColor = getColor theme

addAnnotationBackground :: View.SetLayers a => Theme.ValAnnotation -> AnimId -> a -> a
addAnnotationBackground = addAnnotationBackgroundH Theme.valAnnotationBGColor

addAnnotationHoverBackground :: View.SetLayers a => Theme.ValAnnotation -> AnimId -> a -> a
addAnnotationHoverBackground = addAnnotationBackgroundH Theme.valAnnotationHoverBGColor

data WideAnnotationBehavior
    = ShrinkWideAnnotation
    | HoverWideAnnotation
    | KeepWideAnnotation

wideAnnotationBehaviorFromSelected :: Bool -> WideAnnotationBehavior
wideAnnotationBehaviorFromSelected False = ShrinkWideAnnotation
wideAnnotationBehaviorFromSelected True = HoverWideAnnotation

-- NOTE: Also adds the background color, because it differs based on
-- whether we're hovering
applyWideAnnotationBehavior ::
    Monad m =>
    AnimId -> WideAnnotationBehavior ->
    ExprGuiM m (Vector2 Widget.R -> View -> View)
applyWideAnnotationBehavior animId KeepWideAnnotation =
    do
        theme <- Lens.view Theme.theme <&> Theme.valAnnotation
        addAnnotationBackground theme animId & const & return
applyWideAnnotationBehavior animId ShrinkWideAnnotation =
    Lens.view Theme.theme <&> Theme.valAnnotation
    <&>
    \theme shrinkRatio layout ->
    View.scale shrinkRatio layout
    & addAnnotationBackground theme animId
applyWideAnnotationBehavior animId HoverWideAnnotation =
    do
        theme <- Lens.view Theme.theme <&> Theme.valAnnotation
        shrinker <- applyWideAnnotationBehavior animId ShrinkWideAnnotation
        return $
            \shrinkRatio layout ->
                addAnnotationHoverBackground theme animId layout
                & (`View.hoverInPlaceOf` shrinker shrinkRatio layout)

processAnnotationGui ::
    Monad m =>
    AnimId -> WideAnnotationBehavior ->
    ExprGuiM m (Widget.R -> View -> View)
processAnnotationGui animId wideAnnotationBehavior =
    f
    <$> (Lens.view Theme.theme <&> Theme.valAnnotation)
    <*> Spacer.getSpaceSize
    <*> applyWideAnnotationBehavior animId wideAnnotationBehavior
    where
        f theme stdSpacing applyWide minWidth annotation
            | annotationWidth > minWidth + max shrinkAtLeast expansionLimit
            || heightShrinkRatio < 1 =
                applyWide shrinkRatio annotation
            | otherwise =
                maybeTooNarrow annotation
                & addAnnotationBackground theme animId
            where
                annotationWidth = annotation ^. View.width
                expansionLimit =
                    Theme.valAnnotationWidthExpansionLimit theme & realToFrac
                maxWidth = minWidth + expansionLimit
                shrinkAtLeast = Theme.valAnnotationShrinkAtLeast theme & realToFrac
                heightShrinkRatio =
                    Theme.valAnnotationMaxHeight theme * stdSpacing ^. _2
                    / annotation ^. View.height
                shrinkRatio =
                    annotationWidth - shrinkAtLeast & min maxWidth & max minWidth
                    & (/ annotationWidth) & min heightShrinkRatio & pure
                maybeTooNarrow
                    | minWidth > annotationWidth = View.pad (Vector2 ((minWidth - annotationWidth) / 2) 0)
                    | otherwise = id

data EvalResDisplay = EvalResDisplay
    { erdScope :: ER.ScopeId
    , erdSource :: CurPrevTag
    , erdVal :: ER.Val Type
    }

makeEvaluationResultView ::
    Monad m => AnimId -> EvalResDisplay -> ExprGuiM m (WithTextPos View)
makeEvaluationResultView animId res =
    do
        theme <- Lens.view Theme.theme
        EvalView.make animId (erdVal res)
            <&>
            case erdSource res of
            Current -> id
            Prev -> View.tint (Theme.staleResultTint (Theme.eval theme))

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable)

makeEvalView ::
    Monad m =>
    Maybe (NeighborVals (Maybe EvalResDisplay)) -> EvalResDisplay ->
    AnimId -> ExprGuiM m (WithTextPos View)
makeEvalView mNeighbours evalRes animId =
    do
        theme <- Lens.view Theme.theme
        let Theme.Eval{..} = Theme.eval theme
        let mkAnimId res =
                -- When we can scroll between eval view results we
                -- must encode the scope into the anim ID for smooth
                -- scroll to work.
                -- When we cannot, we'd rather not animate changes
                -- within a scrolled scope (use same animId).
                case mNeighbours of
                Nothing -> animId ++ ["eval-view"]
                Just _ -> animId ++ [encodeS (erdScope res)]
        let makeEvaluationResultViewBG res =
                makeEvaluationResultView (mkAnimId res) res
                <&> addAnnotationBackground (Theme.valAnnotation theme) (mkAnimId res)
                <&> (^. Align.tValue)
        let neighbourView n =
                Lens._Just makeEvaluationResultViewBG n
                <&> Lens.mapped %~ View.scale (neighborsScaleFactor <&> realToFrac)
                <&> Lens.mapped %~ View.pad (neighborsPadding <&> realToFrac)
                <&> fromMaybe View.empty
        (prev, next) <-
            case mNeighbours of
            Nothing -> pure (View.empty, View.empty)
            Just (NeighborVals mPrev mNext) ->
                (,)
                <$> neighbourView mPrev
                <*> neighbourView mNext
        evalView <- makeEvaluationResultView (mkAnimId evalRes) evalRes
        let prevPos = Vector2 0 0.5 * evalView ^. View.size - prev ^. View.size
        let nextPos = Vector2 1 0.5 * evalView ^. View.size
        evalView
            & View.setLayers <>~ View.translateLayers prevPos (prev ^. View.vAnimLayers)
            & View.setLayers <>~ View.translateLayers nextPos (next ^. View.vAnimLayers)
            & return

annotationSpacer :: Monad m => ExprGuiM m View
annotationSpacer = ExprGuiM.vspacer (Theme.valAnnotationSpacing . Theme.valAnnotation)

addAnnotationH ::
    (Functor f, Monad m) =>
    (AnimId -> ExprGuiM m (WithTextPos View)) ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m
    (TreeLayout (f Widget.EventResult) ->
     TreeLayout (f Widget.EventResult))
addAnnotationH f wideBehavior entityId =
    do
        vspace <- annotationSpacer
        annotationLayout <- f animId <&> (^. Align.tValue)
        processAnn <- processAnnotationGui animId wideBehavior
        let onAlignedWidget w =
                w /-/ vspace /-/
-- TODO (ALIGN):
--                AlignTo (w ^. Align.alignmentRatio . _1)
                processAnn (w ^. View.width) annotationLayout
        return $ TreeLayout.alignedWidget %~ onAlignedWidget
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId

addInferredType ::
    (Functor f, Monad m) =>
    Type -> WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (TreeLayout (f Widget.EventResult) -> TreeLayout (f Widget.EventResult))
addInferredType typ = addAnnotationH (TypeView.make typ)

addEvaluationResult ::
    (Functor f, Monad m) =>
    Maybe (NeighborVals (Maybe EvalResDisplay)) -> EvalResDisplay ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m
    (TreeLayout (f Widget.EventResult) ->
     TreeLayout (f Widget.EventResult))
-- REVIEW(Eyal): This is misleading when it refers to Previous results
addEvaluationResult mNeigh resDisp wideBehavior entityId =
    case (erdVal resDisp ^. ER.payload, erdVal resDisp ^. ER.body) of
    (T.TRecord T.CEmpty, _) ->
        addValBGWithColor Theme.evaluatedPathBGColor
    (_, ER.RFunc{}) -> return id
    _ -> addAnnotationH (makeEvalView mNeigh resDisp) wideBehavior entityId

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
    { FocusDelegator.focusChildKeys = [MetaKey noMods GLFW.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Rename"]
    , FocusDelegator.focusParentKeys = [MetaKey noMods GLFW.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Done renaming"]
    }

addDeletionDiagonal :: (Monad m, View.SetLayers a) => ExprGuiM m (Widget.R -> a -> a)
addDeletionDiagonal =
    View.addDiagonal <*> (Lens.view Theme.theme <&> Theme.typeIndicatorErrorColor)

makeNameOriginEdit ::
    Monad m =>
    Name m -> Draw.Color -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeNameOriginEdit name color myId =
    makeNameEdit id name myId
    & styleNameOrigin name color

styleNameOrigin :: Monad m => Name n -> Draw.Color -> ExprGuiM m b -> ExprGuiM m b
styleNameOrigin name color act =
    do
        style <- ExprGuiM.readStyle
        let textEditStyle =
                style
                ^. case nNameSource name of
                    NameSourceAutoGenerated -> Style.styleAutoNameOrigin
                    NameSourceStored -> Style.styleNameOrigin
                & TextEdit.sTextViewStyle . TextView.styleColor .~ color
        act & Reader.local (TextEdit.style .~ textEditStyle)

makeNameEdit ::
    Monad m =>
    (WithTextPos (Widget (T m Widget.EventResult)) ->
     WithTextPos (Widget (T m Widget.EventResult))) ->
    Name m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeNameEdit onActiveEditor (Name nameSrc nameCollision setName name) myId =
    ( FocusDelegator.make ?? nameEditFDConfig
      ?? FocusDelegator.FocusEntryParent ?? myId
      <&> (Align.tValue %~)
    ) <*>
    do
        mCollisionSuffix <- makeCollisionSuffixLabel nameCollision
        makeNameWordEdit
            ?? Property storedName setName
            ?? WidgetIds.nameEditOf myId
            <&> case mCollisionSuffix of
                Nothing -> id
                Just collisionSuffix ->
                    \nameEdit ->
                        (Aligned 0.5 nameEdit /|/ Aligned 0.5 collisionSuffix)
                        ^. Align.value
    & Reader.local (View.animIdPrefix .~ Widget.toAnimId myId)
    <&> onActiveEditor
    where
        empty = TextEdit.EmptyStrings name ""
        storedName =
            case nameSrc of
            NameSourceAutoGenerated -> ""
            NameSourceStored -> name
        makeNameWordEdit =
            TextEdits.makeWordEdit ?? empty
            <&> Lens.mapped . Lens.mapped . Align.tValue . E.eventMap
                %~ E.filterChars (`notElem` disallowedNameChars)

stdWrap ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
stdWrap pl act =
    do
        (res, holePicker) <-
            Reader.local (View.animIdPrefix .~ animId) act
            & ExprGuiM.listenResultPicker
        exprEventMap <- ExprEventMap.make pl holePicker
        maybeAddAnnotationPl pl ?? res
            <&> addEvents exprEventMap
    where
        animId = Widget.toAnimId (WidgetIds.fromExprPayload pl)
        addEvents
            | ExprGuiT.plOfHoleResult pl = E.strongerEvents
            | otherwise = E.weakerEvents

parentDelegator ::
    (MonadReader env m, Config.HasConfig env, Widget.HasCursor env, Applicative f) =>
    Widget.Id -> m (TreeLayout (f Widget.EventResult) -> TreeLayout (f Widget.EventResult))
parentDelegator myId =
    FocusDelegator.make <*> (Lens.view Config.config <&> parentExprFDConfig)
    ?? FocusDelegator.FocusEntryChild ?? WidgetIds.notDelegatingId myId

stdWrapParentExpr ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl mkGui
    | ExprGuiT.plOfHoleResult pl = stdWrap pl mkGui
    | otherwise =
        parentDelegator (WidgetIds.fromExprPayload pl) <*> mkGui & stdWrap pl

grammarLabel :: Monad m => Text -> ExprGuiM m (WithTextPos View)
grammarLabel text =
    do
        theme <- Lens.view Theme.theme
        TextView.makeLabel text
            & Reader.local (TextView.color .~ Theme.grammarColor theme)

addValBG :: (Monad m, View.SetLayers a) => ExprGuiM m (a -> a)
addValBG = addValBGWithColor Theme.valFrameBGColor

addValBGWithColor ::
    (Monad m, View.SetLayers a) =>
    (Theme -> Draw.Color) -> ExprGuiM m (a -> a)
addValBGWithColor color = View.backgroundColor <*> (Lens.view Theme.theme <&> color)

addValPadding :: (Monad m, View.Resizable a) => ExprGuiM m (a -> a)
addValPadding =
    Lens.view Theme.theme <&> Theme.valFramePadding <&> fmap realToFrac
    <&> View.pad

addValFrame :: (Monad m, View.Resizable a) => ExprGuiM m (a -> a)
addValFrame =
    (.)
    <$> addValBG
    <*> addValPadding
    & Reader.local (View.animIdPrefix <>~ ["val"])

-- TODO: This doesn't belong here
makeNameView :: Monad m => Name n -> AnimId -> ExprGuiM m (WithTextPos View)
makeNameView (Name _ collision _ name) animId =
    do
        mSuffixLabel <-
            makeCollisionSuffixLabel collision <&> Lens._Just %~ Aligned 0.5
        TextView.make ?? name ?? animId
            <&> Aligned 0.5
            <&> maybe id (flip (/|/)) mSuffixLabel
            <&> (^. Align.value)
    & Reader.local (View.animIdPrefix .~ animId)

-- TODO: This doesn't belong here
makeCollisionSuffixLabel :: Monad m => NameCollision -> ExprGuiM m (Maybe View)
makeCollisionSuffixLabel NoCollision = return Nothing
makeCollisionSuffixLabel (Collision suffix) =
    do
        theme <- Lens.view Theme.theme
        let Theme.Name{..} = Theme.name theme
        (View.backgroundColor ?? collisionSuffixBGColor)
            <*>
            (TextView.makeLabel (Text.pack (show suffix))
            & Reader.local (TextView.color .~ collisionSuffixTextColor)
            <&> View.scale (realToFrac <$> collisionSuffixScaleFactor))
    <&> (^. Align.tValue)
    <&> Just

maybeAddAnnotationPl ::
    (Functor f, Monad m) =>
    Sugar.Payload x ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
maybeAddAnnotationPl pl =
    do
        wideAnnotationBehavior <-
            if showAnnotation ^. ExprGuiT.showExpanded
            then return KeepWideAnnotation
            else ExprGuiM.isExprSelected pl <&> wideAnnotationBehaviorFromSelected
        maybeAddAnnotation wideAnnotationBehavior
            showAnnotation
            (pl ^. Sugar.plAnnotation)
            (pl ^. Sugar.plEntityId)
    where
        showAnnotation = pl ^. Sugar.plData . ExprGuiT.plShowAnnotation

evaluationResult ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (Maybe (ER.Val Type))
evaluationResult pl =
    ExprGuiM.readMScopeId
    <&> valOfScope (pl ^. Sugar.plAnnotation)
    <&> Lens.mapped %~ erdVal

data EvalAnnotationOptions
    = NormalEvalAnnotation
    | WithNeighbouringEvalAnnotations (NeighborVals (Maybe Sugar.BinderParamScopeId))

maybeAddAnnotation ::
    (Functor f, Monad m) =>
    WideAnnotationBehavior -> ShowAnnotation -> Sugar.Annotation -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
maybeAddAnnotation = maybeAddAnnotationWith NormalEvalAnnotation

data AnnotationMode
    = AnnotationModeNone
    | AnnotationModeTypes
    | AnnotationModeEvaluation (Maybe (NeighborVals (Maybe EvalResDisplay))) EvalResDisplay

getAnnotationMode :: Monad m => EvalAnnotationOptions -> Sugar.Annotation -> ExprGuiM m AnnotationMode
getAnnotationMode opt annotation =
    do
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.None -> return AnnotationModeNone
            CESettings.Types -> return AnnotationModeTypes
            CESettings.Evaluation ->
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
    Sugar.Annotation -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
maybeAddAnnotationWith opt wideAnnotationBehavior ShowAnnotation{..} annotation entityId =
    getAnnotationMode opt annotation
    >>= \case
    AnnotationModeNone
        | _showExpanded -> withType
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
        noAnnotation = pure id
        -- concise mode and eval mode with no result
        inferredType = annotation ^. Sugar.aInferredType
        withType =
            addInferredType inferredType wideAnnotationBehavior entityId
        withVal mNeighborVals scopeAndVal =
            addEvaluationResult mNeighborVals scopeAndVal
            wideAnnotationBehavior entityId

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

listWithDelDests :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
listWithDelDests = ListUtils.withPrevNext

render :: Widget.R -> TreeLayout a -> WithTextPos (Widget a)
render width gui =
    (gui ^. TreeLayout.render)
    TreeLayout.LayoutParams
    { _layoutMode = TreeLayout.LayoutNarrow width
    , _layoutContext = TreeLayout.LayoutClear
    }
