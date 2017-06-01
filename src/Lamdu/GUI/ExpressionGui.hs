{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, RankNTypes, TypeFamilies, LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui
    , egIsFocused
    , render
    -- General:
    , stdHSpace, stdVSpace
    , combine, combineSpaced, combineSpacedMParens
    , (||>), (<||)
    , vboxTopFocal, vboxTopFocalSpaced
    , horizVertFallback
    , tagItem
    , listWithDelDests
    , makeLabel
    , grammarLabel
    , addValFrame, addValPadding
    , addValBGWithColor
    -- Lifted widgets:
    , makeFocusDelegator
    , makeFocusableView
    , makeNameView
    , makeNameEdit, makeNameEditWith
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
import           Graphics.UI.Bottle.Alignment (Alignment(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout(..))
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextEdit.Property as TextEdits
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
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

{-# INLINE egIsFocused #-}
egIsFocused :: TreeLayout a -> Bool
-- TODO: Fix this:
egIsFocused tl =
    (tl ^. TreeLayout.render) params ^. AlignedWidget.widget & Widget.isFocused
    where
        params =
            TreeLayout.LayoutParams
            { _layoutMode = TreeLayout.LayoutWide
            , _layoutContext = TreeLayout.LayoutClear
            }

maybeIndent :: Maybe ParenIndentInfo -> TreeLayout a -> TreeLayout a
maybeIndent Nothing = id
maybeIndent (Just piInfo) =
    TreeLayout.render %~ f
    where
        f mkLayout lp =
            case lp ^. TreeLayout.layoutContext of
            TreeLayout.LayoutVertical ->
                content
                & AlignedWidget.addBefore AlignedWidget.Horizontal
                    [ Spacer.make
                        (Vector2 barWidth (content ^. AlignedWidget.widget . Widget.height))
                        & Widget.fromView
                        & Widget.backgroundColor bgAnimId
                          (Theme.indentBarColor indentConf)
                        & AlignedWidget.fromCenteredWidget
                        & AlignedWidget.alignment . _2 .~ 0
                    , Spacer.make (Vector2 gapWidth 0)
                        & Widget.fromView & AlignedWidget.fromCenteredWidget
                    ]
                where
                    indentConf = piIndentTheme piInfo
                    stdSpace = piStdHorizSpacing piInfo
                    barWidth = stdSpace * Theme.indentBarWidth indentConf
                    gapWidth = stdSpace * Theme.indentBarGap indentConf
                    indentWidth = barWidth + gapWidth
                    content =
                        lp & TreeLayout.layoutMode . TreeLayout.modeWidths -~ indentWidth
                        & mkLayout
                        & AlignedWidget.alignment . _2 .~ 0
                    bgAnimId = piAnimId piInfo ++ ["("]
            _ -> mkLayout lp

vboxTopFocal :: [TreeLayout a] -> TreeLayout a
vboxTopFocal [] = TreeLayout.empty
vboxTopFocal (gui:guis) =
    TreeLayout.render #
    \layoutParams ->
    let cp =
            TreeLayout.LayoutParams
            { _layoutMode = layoutParams ^. TreeLayout.layoutMode
            , _layoutContext = TreeLayout.LayoutVertical
            }
    in
    cp
    & gui ^. TreeLayout.render
    & AlignedWidget.addAfter AlignedWidget.Vertical
        (guis ^.. Lens.traverse . TreeLayout.render ?? cp)

vboxTopFocalSpaced ::
    Monad m => ExprGuiM m ([TreeLayout a] -> TreeLayout a)
vboxTopFocalSpaced =
    stdVSpace
    <&> TreeLayout.fromCenteredWidget
    <&> List.intersperse
    <&> fmap vboxTopFocal

hCombine ::
    (AlignedWidget.Orientation -> [AlignedWidget a] -> AlignedWidget a ->
     AlignedWidget a) ->
    AlignedWidget a -> TreeLayout a -> TreeLayout a
hCombine f layout gui =
    TreeLayout.render #
    \layoutParams ->
    TreeLayout.LayoutParams
    { _layoutMode =
        layoutParams ^. TreeLayout.layoutMode
        & TreeLayout.modeWidths -~ layout ^. AlignedWidget.width
    , _layoutContext = TreeLayout.LayoutHorizontal
    }
    & gui ^. TreeLayout.render
    & f AlignedWidget.Horizontal [layout]

(||>) :: AlignedWidget a -> TreeLayout a -> TreeLayout a
(||>) = hCombine AlignedWidget.addBefore

(<||) :: TreeLayout a -> AlignedWidget a -> TreeLayout a
(<||) = flip (hCombine AlignedWidget.addAfter)

stdHSpace :: Monad m => ExprGuiM m (Widget a)
stdHSpace =
    ExprGuiM.widgetEnv BWidgets.stdHSpaceView
    <&> Widget.fromView

stdVSpace :: Monad m => ExprGuiM m (Widget a)
stdVSpace =
    ExprGuiM.widgetEnv BWidgets.stdVSpaceView
    <&> Widget.fromView

data ParenIndentInfo = ParenIndentInfo
    { piAnimId :: AnimId
    , piTextStyle :: TextView.Style
    , piIndentTheme :: Theme.Indent
    , piStdHorizSpacing :: Widget.R
    }

parenLabel :: ParenIndentInfo -> Text -> AlignedWidget a
parenLabel parenInfo t =
    TextView.make (piTextStyle parenInfo) t
    (piAnimId parenInfo ++ [encodeUtf8 t])
    & Widget.fromView & AlignedWidget.fromCenteredWidget

horizVertFallback ::
    Monad m =>
    Maybe AnimId -> ExprGuiM m (TreeLayout a -> TreeLayout a -> TreeLayout a)
horizVertFallback mParenId =
    mParenId & Lens._Just %%~ makeParenIndentInfo
    <&> horizVertFallbackH

horizVertFallbackH ::
    Maybe ParenIndentInfo -> TreeLayout a -> TreeLayout a -> TreeLayout a
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
            wide
            & AlignedWidget.addBefore AlignedWidget.Horizontal [parenLabel parenInfo "("]
            & AlignedWidget.addAfter AlignedWidget.Horizontal [parenLabel parenInfo ")"]
        _ -> wide
    TreeLayout.LayoutNarrow limit
        | wide ^. AlignedWidget.width > limit ->
            layoutParams & maybeIndent mParenInfo vert ^. TreeLayout.render
        | otherwise -> wide

combineWith ::
    Maybe ParenIndentInfo ->
    ([AlignedWidget a] -> [AlignedWidget a]) ->
    ([TreeLayout a] -> [TreeLayout a]) ->
    [TreeLayout a] -> TreeLayout a
combineWith mParenInfo onHGuis onVGuis guis =
    horizVertFallbackH mParenInfo wide vert
    where
        vert = vboxTopFocal (onVGuis guis <&> TreeLayout.alignment . _1 .~ 0)
        wide =
            guis ^.. Lens.traverse . TreeLayout.render
            ?? TreeLayout.LayoutParams
                { _layoutMode = TreeLayout.LayoutWide
                , _layoutContext = TreeLayout.LayoutHorizontal
                }
            & onHGuis
            & AlignedWidget.hbox 0.5
            & TreeLayout.fromAlignedWidget

combine :: [TreeLayout a] -> TreeLayout a
combine = combineWith Nothing id id

makeParenIndentInfo :: Monad m => AnimId -> ExprGuiM m ParenIndentInfo
makeParenIndentInfo parensId =
    do
        textStyle <-
            ExprGuiM.widgetEnv WE.readTextStyle
            <&> (^. TextEdit.sTextViewStyle)
        theme <- ExprGuiM.readTheme <&> Theme.indent
        stdSpacing <- ExprGuiM.widgetEnv BWidgets.stdSpacing <&> (^. _1)
        ParenIndentInfo parensId textStyle theme stdSpacing & return

combineSpaced :: Monad m => ExprGuiM m ([TreeLayout a] -> TreeLayout a)
combineSpaced = combineSpacedMParens Nothing

combineSpacedMParens ::
    Monad m => Maybe AnimId -> ExprGuiM m ([TreeLayout a] -> TreeLayout a)
combineSpacedMParens mParensId =
    do
        hSpace <- stdHSpace <&> AlignedWidget.fromCenteredWidget
        vSpace <- stdVSpace <&> TreeLayout.fromCenteredWidget
        mParenInfo <- mParensId & Lens._Just %%~ makeParenIndentInfo
        return $ combineWith mParenInfo (List.intersperse hSpace) (List.intersperse vSpace)

tagItem ::
    Monad m => ExprGuiM m (AlignedWidget a -> TreeLayout a -> TreeLayout a)
tagItem =
    stdHSpace <&> AlignedWidget.fromCenteredWidget <&> f
    where
        f space tag item =
            tag ||> (space ||> (item & TreeLayout.alignment . _1 .~ 0))

addAnnotationBackgroundH ::
    (Theme.ValAnnotation -> Draw.Color) -> Theme.ValAnnotation -> AnimId ->
    View -> View
addAnnotationBackgroundH getColor theme animId =
    View.backgroundColor bgAnimId bgColor
    where
        bgAnimId = animId ++ ["annotation background"]
        bgColor = getColor theme

addAnnotationBackground :: Theme.ValAnnotation -> AnimId -> View -> View
addAnnotationBackground = addAnnotationBackgroundH Theme.valAnnotationBGColor

addAnnotationHoverBackground :: Theme.ValAnnotation -> AnimId -> View -> View
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
    ExprGuiM m (Vector2 Widget.R -> AlignedWidget a -> AlignedWidget a)
applyWideAnnotationBehavior animId KeepWideAnnotation =
    do
        theme <- ExprGuiM.readTheme <&> Theme.valAnnotation
        AlignedWidget.widget . Widget.view %~ addAnnotationBackground theme animId
            & const & return
applyWideAnnotationBehavior animId ShrinkWideAnnotation =
    ExprGuiM.readTheme <&> Theme.valAnnotation
    <&>
    \theme shrinkRatio layout ->
    AlignedWidget.scaleAround (Alignment 0) shrinkRatio layout
    & AlignedWidget.widget . Widget.view %~ addAnnotationBackground theme animId
applyWideAnnotationBehavior animId HoverWideAnnotation =
    do
        theme <- ExprGuiM.readTheme <&> Theme.valAnnotation
        shrinker <- applyWideAnnotationBehavior animId ShrinkWideAnnotation
        return $
            \shrinkRatio layout ->
                layout
                & AlignedWidget.widget . Widget.view %~ addAnnotationHoverBackground theme animId
                & (`AlignedWidget.hoverInPlaceOf` shrinker shrinkRatio layout)

processAnnotationGui ::
    Monad m =>
    AnimId -> WideAnnotationBehavior ->
    ExprGuiM m (Widget.R -> AlignedWidget a -> AlignedWidget a)
processAnnotationGui animId wideAnnotationBehavior =
    f
    <$> (ExprGuiM.readTheme <&> Theme.valAnnotation)
    <*> ExprGuiM.widgetEnv BWidgets.stdSpacing
    <*> applyWideAnnotationBehavior animId wideAnnotationBehavior
    where
        f theme stdSpacing applyWide minWidth annotationLayout
            | annotationWidth > minWidth + max shrinkAtLeast expansionLimit
            || heightShrinkRatio < 1 =
                applyWide shrinkRatio annotationLayout
            | otherwise =
                maybeTooNarrow annotationLayout
                & AlignedWidget.widget . Widget.view %~ addAnnotationBackground theme animId
            where
                annotationWidth = annotationLayout ^. AlignedWidget.width
                expansionLimit =
                    Theme.valAnnotationWidthExpansionLimit theme & realToFrac
                maxWidth = minWidth + expansionLimit
                shrinkAtLeast = Theme.valAnnotationShrinkAtLeast theme & realToFrac
                heightShrinkRatio =
                    Theme.valAnnotationMaxHeight theme * stdSpacing ^. _2
                    / annotationLayout ^. AlignedWidget.widget . Widget.height
                shrinkRatio =
                    annotationWidth - shrinkAtLeast & min maxWidth & max minWidth
                    & (/ annotationWidth) & min heightShrinkRatio & pure
                maybeTooNarrow
                    | minWidth > annotationWidth = AlignedWidget.pad (Vector2 ((minWidth - annotationWidth) / 2) 0)
                    | otherwise = id

data EvalResDisplay = EvalResDisplay
    { erdScope :: ER.ScopeId
    , erdSource :: CurPrevTag
    , erdVal :: ER.Val Type
    }

makeEvaluationResultView ::
    Monad m => AnimId -> EvalResDisplay -> ExprGuiM m (AlignedWidget a)
makeEvaluationResultView animId res =
    do
        theme <- ExprGuiM.readTheme
        view <- EvalView.make animId (erdVal res)
        view
            & case erdSource res of
            Current -> id
            Prev -> View.tint (Theme.staleResultTint (Theme.eval theme))
            & return
    <&> Widget.fromView
    <&> AlignedWidget.fromCenteredWidget

makeTypeView :: Monad m => Type -> AnimId -> ExprGuiM m (AlignedWidget f)
makeTypeView typ animId =
    TypeView.make typ animId <&> AlignedWidget.fromCenteredWidget . Widget.fromView

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable)

makeEvalView ::
    Monad m =>
    Maybe (NeighborVals (Maybe EvalResDisplay)) -> EvalResDisplay ->
    AnimId -> ExprGuiM m (AlignedWidget a)
makeEvalView mNeighbours evalRes animId =
    do
        theme <- ExprGuiM.readTheme
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
                <&> AlignedWidget.widget . Widget.view %~
                    addAnnotationBackground (Theme.valAnnotation theme) (mkAnimId res)
        let neighbourViews n yPos =
                n ^.. Lens._Just
                <&> makeEvaluationResultViewBG
                <&> Lens.mapped %~
                    AlignedWidget.pad (neighborsPadding <&> realToFrac) .
                    AlignedWidget.scale (neighborsScaleFactor <&> realToFrac)
                <&> Lens.mapped . AlignedWidget.alignment . _2 .~ yPos
        (prevs, nexts) <-
            case mNeighbours of
            Nothing -> ([], []) & pure
            Just (NeighborVals mPrev mNext) ->
                (,)
                <$> sequence (neighbourViews mPrev 1)
                <*> sequence (neighbourViews mNext 0)
        evalView <- makeEvaluationResultView (mkAnimId evalRes) evalRes
        evalView
            & AlignedWidget.addBefore AlignedWidget.Horizontal prevs
            & AlignedWidget.addAfter AlignedWidget.Horizontal nexts
            & (`AlignedWidget.hoverInPlaceOf` evalView)
            & return

annotationSpacer :: Monad m => ExprGuiM m (AlignedWidget a)
annotationSpacer =
    ExprGuiM.vspacer (Theme.valAnnotationSpacing . Theme.valAnnotation)
    <&> AlignedWidget.fromCenteredWidget

addAnnotationH ::
    Monad m =>
    (AnimId -> ExprGuiM m (AlignedWidget a)) ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (TreeLayout a -> TreeLayout a)
addAnnotationH f wideBehavior entityId =
    do
        vspace <- annotationSpacer
        annotationLayout <- f animId
        processAnn <- processAnnotationGui animId wideBehavior
        return $
            \tl ->
            TreeLayout.render # \lp ->
            let layout = lp & tl ^. TreeLayout.render
            in  layout
                & AlignedWidget.addAfter AlignedWidget.Vertical
                [ vspace
                , processAnn (layout ^. AlignedWidget.width) annotationLayout
                     & AlignedWidget.alignment . _1 .~ layout ^. AlignedWidget.alignment . _1
                ]
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId

addInferredType ::
    Monad m =>
    Type -> WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (TreeLayout a -> TreeLayout a)
addInferredType typ = addAnnotationH (makeTypeView typ)

addEvaluationResult ::
    Monad m =>
    Maybe (NeighborVals (Maybe EvalResDisplay)) -> EvalResDisplay ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (TreeLayout a -> TreeLayout a)
-- REVIEW(Eyal): This is misleading when it refers to Previous results
addEvaluationResult mNeigh resDisp wideBehavior entityId =
    case (erdVal resDisp ^. ER.payload, erdVal resDisp ^. ER.body) of
    (T.TRecord T.CEmpty, _) ->
        addValBGWithColor Theme.evaluatedPathBGColor (WidgetIds.fromEntityId entityId)
        <&> (TreeLayout.widget %~)
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

addDeletionDiagonal :: Monad m => ExprGuiM m (Widget.R -> AnimId -> View -> View)
addDeletionDiagonal =
    do
        theme <- ExprGuiM.readTheme
        return $ \thickness animId ->
            View.addDiagonal thickness (animId ++ ["diagonal"])
            (Theme.typeIndicatorErrorColor theme)

makeNameOriginEdit ::
    Monad m =>
    Name m -> Draw.Color -> Widget.Id ->
    ExprGuiM m (Widget (T m Widget.EventResult))
makeNameOriginEdit name color myId =
    makeNameEdit name myId
    & styleNameOrigin name color

styleNameOrigin :: Monad m => Name n -> Draw.Color -> ExprGuiM m b -> ExprGuiM m b
styleNameOrigin name color act =
    do
        style <- ExprGuiM.readStyle
        let textEditStyle =
                case nNameSource name of
                NameSourceAutoGenerated -> Style.styleAutoNameOrigin style
                NameSourceStored -> Style.styleNameOrigin style
                & TextEdit.sTextViewStyle . TextView.styleColor .~ color
        act & ExprGuiM.localEnv (WE.envTextStyle .~ textEditStyle)

makeNameEdit ::
    Monad m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m Widget.EventResult))
makeNameEdit = makeNameEditWith id

makeNameEditWith ::
    Monad m =>
    (Widget (T m Widget.EventResult) -> Widget (T m Widget.EventResult)) ->
    Name m -> Widget.Id -> ExprGuiM m (Widget (T m Widget.EventResult))
makeNameEditWith onActiveEditor (Name nameSrc nameCollision setName name) myId =
    ExprGuiM.makeFocusDelegator nameEditFDConfig
    FocusDelegator.FocusEntryParent myId
    <*>
    do
        mCollisionSuffix <-
            makeCollisionSuffixLabel nameCollision (Widget.toAnimId myId)
            <&> Lens._Just %~ Widget.fromView
        nameEdit <-
            makeNameWordEdit ?? Property storedName setName ?? WidgetIds.nameEditOf myId
            & ExprGuiM.widgetEnv
        return . Box.hboxCentered $ nameEdit : mCollisionSuffix ^.. Lens._Just
    <&> onActiveEditor
    where
        empty = TextEdit.EmptyStrings name ""
        storedName =
            case nameSrc of
            NameSourceAutoGenerated -> ""
            NameSourceStored -> name
        makeNameWordEdit =
            TextEdits.makeWordEdit ?? empty
            <&> Lens.mapped . Lens.mapped . Widget.eventMap
                %~ E.filterChars (`notElem` disallowedNameChars)

stdWrap ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
stdWrap pl act =
    do
        (res, holePicker) <- ExprGuiM.listenResultPicker act
        exprEventMap <- ExprEventMap.make pl holePicker
        maybeAddAnnotationPl pl ?? res
            <&> TreeLayout.widget %~ addEvents exprEventMap
    where
        addEvents
            | ExprGuiT.plOfHoleResult pl = Widget.strongerEvents
            | otherwise = Widget.weakerEvents

makeFocusDelegator ::
    (Monad m, Monad f) =>
    FocusDelegator.Config ->
    FocusDelegator.FocusEntryTarget ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
makeFocusDelegator =
    ExprGuiM.makeFocusDelegator
    <&> Lens.mapped . Lens.mapped . Lens.mapped %~ (TreeLayout.widget %~)

parentDelegator ::
    (Monad f, Monad m) => Widget.Id ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
parentDelegator myId =
    do
        config <- ExprGuiM.readConfig
        makeFocusDelegator (parentExprFDConfig config)
            FocusDelegator.FocusEntryChild (WidgetIds.notDelegatingId myId)

stdWrapParentExpr ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl mkGui =
    parentDelegator myId <*> mkGui innerId
    & stdWrap pl
    & ExprGuiM.assignCursor myId innerId
    where
        myId = WidgetIds.fromExprPayload pl
        innerId = WidgetIds.delegatingId myId

makeFocusableView ::
    (Applicative f, Monad m) =>
    Widget.Id ->
    ExprGuiM m
    ( AlignedWidget (f Widget.EventResult) ->
      AlignedWidget (f Widget.EventResult)
    )
makeFocusableView myId =
    Widget.makeFocusableView ?? myId
    <&> (AlignedWidget.widget %~)

makeLabel :: Monad m => Text -> AnimId -> ExprGuiM m (AlignedWidget a)
makeLabel text animId = ExprGuiM.makeLabel text animId <&> Widget.fromView <&> AlignedWidget.fromCenteredWidget

grammarLabel :: Monad m => Text -> AnimId -> ExprGuiM m (AlignedWidget f)
grammarLabel text animId =
    do
        theme <- ExprGuiM.readTheme
        makeLabel text animId
            & ExprGuiM.localEnv (WE.textColor .~ Theme.grammarColor theme)

addValBG :: Monad m => Widget.Id -> ExprGuiM m (Widget f -> Widget f)
addValBG = addValBGWithColor Theme.valFrameBGColor

addValBGWithColor ::
    Monad m =>
    (Theme -> Draw.Color) -> Widget.Id -> ExprGuiM m (Widget f -> Widget f)
addValBGWithColor color myId =
    do
        theme <- ExprGuiM.readTheme
        Widget.backgroundColor animId (color theme) & return
    where
        animId = Widget.toAnimId myId ++ ["val"]

addValPadding :: Monad m => ExprGuiM m (TreeLayout a -> TreeLayout a)
addValPadding =
    ExprGuiM.readTheme <&> Theme.valFramePadding <&> fmap realToFrac
    <&> TreeLayout.pad

addValFrame ::
    Monad m => Widget.Id -> ExprGuiM m (TreeLayout a -> TreeLayout a)
addValFrame myId =
    (.)
    <$> (addValBG myId <&> (TreeLayout.widget %~))
    <*> addValPadding

-- TODO: This doesn't belong here
makeNameView :: Monad m => Name n -> AnimId -> ExprGuiM m View
makeNameView (Name _ collision _ name) animId =
    do
        nameView <- TextView.make ?? name ?? animId & ExprGuiM.widgetEnv
        mSuffixLabel <- makeCollisionSuffixLabel collision $ animId ++ ["suffix"]
        GridView.horizontalAlign 0.5 (nameView : mSuffixLabel ^.. Lens._Just) & return

-- TODO: This doesn't belong here
makeCollisionSuffixLabel :: Monad m => NameCollision -> AnimId -> ExprGuiM m (Maybe View)
makeCollisionSuffixLabel NoCollision _ = return Nothing
makeCollisionSuffixLabel (Collision suffix) animId =
    do
        theme <- ExprGuiM.readTheme
        let Theme.Name{..} = Theme.name theme
        TextView.makeLabel ?? Text.pack (show suffix) ?? animId
            & WE.localEnv (WE.textColor .~ collisionSuffixTextColor)
            <&> View.scale (realToFrac <$> collisionSuffixScaleFactor)
            <&> View.backgroundColor animId collisionSuffixBGColor
            <&> Just
            & ExprGuiM.widgetEnv

maybeAddAnnotationPl ::
    Monad m =>
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
    Monad m =>
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
    Monad m =>
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

render :: Widget.R -> TreeLayout a -> AlignedWidget a
render width gui =
    (gui ^. TreeLayout.render)
    TreeLayout.LayoutParams
    { _layoutMode = TreeLayout.LayoutNarrow width
    , _layoutContext = TreeLayout.LayoutClear
    }
