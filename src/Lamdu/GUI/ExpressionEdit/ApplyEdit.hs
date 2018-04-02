{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( makeSimple, makeLabeled
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

infixMarker :: Vector2 Anim.R -> Draw.Image
infixMarker (Vector2 w h) =
    mconcat
    [ Draw.line (x, 0) (0,x)
    , Draw.line (w-x, 0) (w,x)
    , Draw.line (w-x, h) (w,h-x)
    , Draw.line (x, h) (0,h-x)
    , Draw.line (0, x) (0, h-x)
    , Draw.line (w, x) (w, h-x)
    , Draw.line (x, 0) (w-x, 0)
    , Draw.line (x, h) (w-x, h)
    ]
    <&> const ()
    where
        x = min w h / 4

addInfixMarker :: Element a => Widget.Id -> a -> a
addInfixMarker widgetId =
    Element.bottomLayer %@~
    \size -> Anim.singletonFrame 1 frameId (infixMarker size) & flip mappend
    where
        frameId = Widget.toAnimId widgetId ++ ["infix"]

makeFuncVar ::
    Monad m =>
    NearestHoles -> Sugar.BinderVarRef (Name (T m)) (T m) -> Widget.Id ->
    ExprGuiM (T m) (WithTextPos (Widget (T m GuiState.Update)))
makeFuncVar nearestHoles funcVar myId =
    do
        jump <- ExprEventMap.jumpHolesEventMap nearestHoles
        GetVarEdit.makeGetBinder funcVar funcId
            <&> Align.tValue %~ Widget.weakerEvents jump
    where
        funcId = Widget.joinId myId ["Func"]

makeInfixFuncName ::
    Monad m =>
    NearestHoles -> Sugar.BinderVarRef (Name (T m)) (T m) -> Widget.Id ->
    ExprGuiM (T m) (WithTextPos (Widget (T m GuiState.Update)))
makeInfixFuncName nearestHoles funcVar myId =
    makeFuncVar nearestHoles funcVar myId <&> mAddMarker
    where
        nameText =
            Name.visible (funcVar ^. Sugar.bvNameRef . Sugar.nrName)
            ^. _1 . Name.ttText
        mAddMarker
            | Lens.allOf Lens.each (`elem` Chars.operator) nameText = id
            | otherwise = addInfixMarker myId

isBoxed :: Sugar.LabeledApply name binderVar a -> Bool
isBoxed apply =
    Lens.has (Sugar.aAnnotatedArgs . traverse) apply
    || Lens.has (Sugar.aRelayedArgs . traverse) apply

makeFuncRow ::
    Monad m =>
    Maybe AnimId ->
    Sugar.LabeledApply (Name (T m)) (T m) (ExprGui.SugarExpr (T m)) ->
    NearestHoles ->
    Widget.Id ->
    ExprGuiM (T m) (ExpressionGui (T m))
makeFuncRow mParensId apply applyNearestHoles myId =
    case apply ^. Sugar.aSpecialArgs of
    Sugar.Verbose ->
        makeFuncVar nextHoles funcVar
        myId <&> Responsive.fromWithTextPos
        where
            nextHoles =
                case apply ^. Sugar.aAnnotatedArgs of
                [] -> applyNearestHoles -- all args are relayed args
                (x:_) -> x ^. Sugar.aaExpr & ExprGui.nextHolesBefore
    Sugar.Object arg ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ makeFuncVar (ExprGui.nextHolesBefore arg) funcVar myId
            <&> Responsive.fromWithTextPos
        , ExprGuiM.makeSubexpression arg
        ]
    Sugar.Infix l r ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ (Options.boxSpaced ?? Options.disambiguationNone)
            <*> sequenceA
            [ ExprGuiM.makeSubexpression l
            , makeInfixFuncName (ExprGui.nextHolesBefore r) funcVar myId
                <&> Responsive.fromWithTextPos
            ]
        , ExprGuiM.makeSubexpression r
        ]
    where
        funcVar = apply ^. Sugar.aFunc

makeLabeled ::
    Monad m =>
    Sugar.LabeledApply (Name (T m)) (T m) (ExprGui.SugarExpr (T m)) ->
    Sugar.Payload (Name (T m)) (T m) ExprGui.Payload ->
    ExprGuiM (T m) (ExpressionGui (T m))
makeLabeled apply pl =
    stdWrapParentExpr pl
    <*> ( makeFuncRow mParensId apply
            (pl ^. Sugar.plData . ExprGui.plNearestHoles) myId
            >>= addBox
        )
    where
        addBox
            | isBoxed apply = mkBoxed apply (pl ^. Sugar.plData . ExprGui.plNearestHoles)
            | otherwise = pure
        mParensId
            | needParens = Just (Widget.toAnimId myId)
            | otherwise = Nothing
        needParens = pl ^. Sugar.plData . ExprGui.plNeedParens
        myId = WidgetIds.fromExprPayload pl

makeArgRow ::
    Monad m =>
    Sugar.AnnotatedArg (Name (T m)) (ExprGui.SugarExpr (T m)) ->
    ExprGuiM (T m) (Responsive.TaggedItem (T m GuiState.Update))
makeArgRow arg =
    do
        argTag <- TagEdit.makeArgTag (arg ^. Sugar.aaTag . Sugar.tagName) (arg ^. Sugar.aaTag . Sugar.tagInstance)
        space <- Spacer.stdHSpace
        expr <- ExprGuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        pure Responsive.TaggedItem
            { Responsive._tagPre = argTag /|/ space <&> Widget.fromView
            , Responsive._taggedItem = expr
            , Responsive._tagPost = Element.empty
            }

mkRelayedArgs :: Monad m => NearestHoles -> [Sugar.RelayedArg (Name (T m)) (T m)] -> ExprGuiM (T m) (ExpressionGui (T m))
mkRelayedArgs nearestHoles args =
    do
        argEdits <- mapM makeArgEdit args
        collapsed <- Styled.grammarLabel "➾" <&> Responsive.fromTextView
        Options.boxSpaced ?? Options.disambiguationNone ?? collapsed : argEdits
    where
        makeArgEdit arg =
            ExprEventMap.addWith ExprEventMap.defaultOptions
            ExprEventMap.ExprInfo
            { exprInfoActions = arg ^. Sugar.raActions
            , exprInfoNearestHoles = nearestHoles
            , exprInfoIsHoleResult = False
            , exprInfoMinOpPrec = 0
            , exprInfoIsSelected = True
            }
            <*> GetVarEdit.makeNoActions (arg ^. Sugar.raValue) (WidgetIds.fromEntityId (arg ^. Sugar.raId))

mkBoxed ::
    Monad m =>
    Sugar.LabeledApply (Name (T m)) (T m) (ExprGui.SugarExpr (T m)) ->
    NearestHoles ->
    ExpressionGui (T m) ->
    ExprGuiM (T m) (ExpressionGui (T m))
mkBoxed apply nearestHoles funcRow =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> pure []
            xs ->
                Responsive.taggedList
                <*> traverse makeArgRow xs
                <&> (:[])
        relayedArgs <-
            case apply ^. Sugar.aRelayedArgs of
            [] -> pure []
            args -> mkRelayedArgs nearestHoles args <&> (:[])
        Styled.addValFrame
            <*> (Responsive.vboxSpaced ?? (funcRow : argRows ++ relayedArgs))

makeSimple ::
    Monad m =>
    Sugar.Apply (ExprGui.SugarExpr (T m)) ->
    Sugar.Payload (Name (T m)) (T m) ExprGui.Payload ->
    ExprGuiM (T m) (ExpressionGui (T m))
makeSimple (Sugar.Apply func arg) pl =
    stdWrapParentExpr pl
    <*> ( (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
            <*> sequenceA
            [ ExprGuiM.makeSubexpression func
            , ExprGuiM.makeSubexpression arg
            ]
        )
    where
        mParensId
            | pl ^. Sugar.plData . ExprGui.plNeedParens = Just (Widget.toAnimId myId)
            | otherwise = Nothing
        myId = WidgetIds.fromExprPayload pl
