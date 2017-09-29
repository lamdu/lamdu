{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( makeSimple, makeLabeled, prefixPrecedence
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.CharClassification as CharClassification
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Names.Types as Name
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

prefixPrecedence :: Int
prefixPrecedence = 10

mkPrecedence :: Sugar.LabeledApply (Name m) p expr -> Int
mkPrecedence apply =
    case apply ^. Sugar.aSpecialArgs of
    Sugar.NoSpecialArgs -> 0
    Sugar.ObjectArg{} -> prefixPrecedence
    Sugar.InfixArgs _ _ ->
        case funcName of
        x:_ -> CharClassification.precedence x
        _ -> 20
    where
        (visibleName, _mCollision) =
            apply ^. Sugar.aFunc . Sugar.bvNameRef . Sugar.nrName . Name.form
            & Name.visible
        funcName = Text.unpack visibleName

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
    NearestHoles -> Sugar.BinderVar (Name m) m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (Transaction m Widget.EventResult)))
makeFuncVar nearestHoles funcVar myId =
    do
        jump <- ExprEventMap.jumpHolesEventMap nearestHoles
        GetVarEdit.makeGetBinder funcVar myId
            <&> Align.tValue %~ E.weakerEvents jump

makeInfixFuncName ::
    Monad m =>
    NearestHoles -> Sugar.BinderVar (Name m) m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (Transaction m Widget.EventResult)))
makeInfixFuncName nearestHoles funcVar myId =
    makeFuncVar nearestHoles funcVar myId <&> mAddMarker
    where
        mAddMarker
            | funcVar ^. Sugar.bvNameRef . Sugar.nrName . Name.form
              & BinderEdit.nonOperatorName =
                addInfixMarker myId
            | otherwise = id

isBoxed :: Sugar.LabeledApply name binderVar a -> Bool
isBoxed apply =
    Lens.has (Sugar.aAnnotatedArgs . traverse) apply
    || Lens.has (Sugar.aRelayedArgs . traverse) apply

makeFuncRow ::
    Monad m =>
    Maybe AnimId ->
    Int ->
    Sugar.LabeledApply (Name m) m (ExprGuiT.SugarExpr m) ->
    NearestHoles ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeFuncRow mParensId prec apply applyNearestHoles myId =
    case apply ^. Sugar.aSpecialArgs of
    Sugar.NoSpecialArgs ->
        makeFuncVar nextHoles funcVar
        myId <&> Responsive.fromWithTextPos
        where
            nextHoles =
                case apply ^. Sugar.aAnnotatedArgs of
                [] -> applyNearestHoles -- all args are relayed args
                (x:_) -> x ^. Sugar.aaExpr & ExprGuiT.nextHolesBefore
    Sugar.ObjectArg arg ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ makeFuncVar (ExprGuiT.nextHolesBefore arg) funcVar myId
            <&> Responsive.fromWithTextPos
        , ExprGuiM.makeSubexpressionWith
          (if isBoxed apply then 0 else prec)
          (ExpressionGui.before .~ prec) arg
        ]
    Sugar.InfixArgs l r ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ (Options.boxSpaced ?? Options.disambiguationNone)
            <*> sequenceA
            [ ExprGuiM.makeSubexpressionWith 0 (ExpressionGui.after .~ prec) l
            , makeInfixFuncName (ExprGuiT.nextHolesBefore r) funcVar myId
                <&> Responsive.fromWithTextPos
            ]
        , ExprGuiM.makeSubexpressionWith (prec+1) (ExpressionGui.before .~ prec+1) r
        ]
    where
        funcVar = apply ^. Sugar.aFunc

makeLabeled ::
    Monad m =>
    Sugar.LabeledApply (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeLabeled apply pl =
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let needParens =
                not (isBoxed apply)
                && Prec.needParens parentPrec (Prec.my prec)
        let fixPrec
                | needParens = ExprGuiM.withLocalPrecedence 0 (const (Prec.make 0))
                | otherwise = id
        let mParensId
                | needParens = Just (Widget.toAnimId myId)
                | otherwise = Nothing
        let addBox
                | isBoxed apply = mkBoxed apply (pl ^. Sugar.plData . ExprGuiT.plNearestHoles)
                | otherwise = id
        makeFuncRow mParensId prec apply (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) myId
            & addBox
            & ExpressionGui.stdWrapParentExpr pl (pl ^. Sugar.plEntityId)
            & fixPrec
    where
        prec = mkPrecedence apply
        myId = WidgetIds.fromExprPayload pl

makeArgRow ::
    Monad m =>
    Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (WithTextPos View, ExpressionGui m)
makeArgRow arg =
    do
        argTag <- TagEdit.makeArgTag (arg ^. Sugar.aaName) (arg ^. Sugar.aaTag . Sugar.tagInstance)
        space <- Spacer.stdHSpace
        expr <- ExprGuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        return (argTag /|/ space, expr)

mkRelayedArgs :: Monad m => NearestHoles -> [Sugar.RelayedArg (Name m) m] -> ExprGuiM m (ExpressionGui m)
mkRelayedArgs nearestHoles args =
    do
        argEdits <- mapM makeArgEdit args
        collapsed <- ExpressionGui.grammarLabel "➾" <&> Responsive.fromTextView
        Options.boxSpaced ?? Options.disambiguationNone ?? collapsed : argEdits
    where
        makeArgEdit arg =
            do
                eventMap <-
                    ExprEventMap.makeWith ExprEventMap.ExprInfo
                    { exprInfoActions = arg ^. Sugar.raActions
                    , exprInfoEntityId = arg ^. Sugar.raId
                    , exprInfoNearestHoles = nearestHoles
                    , exprInfoIsHoleResult = False
                    } ExprGuiM.NoHolePick
                GetVarEdit.makeGetParam (arg ^. Sugar.raValue) (WidgetIds.fromEntityId (arg ^. Sugar.raId))
                    <&> Responsive.fromWithTextPos
                    <&> E.weakerEvents eventMap

mkBoxed ::
    Monad m =>
    Sugar.LabeledApply (Name m) m (ExprGuiT.SugarExpr m) ->
    NearestHoles ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
mkBoxed apply nearestHoles mkFuncRow =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> return []
            xs ->
                Responsive.taggedList
                <*> (traverse makeArgRow xs <&> Lens.mapped . _1 . Align.tValue %~ Widget.fromView) <&> (:[])
        funcRow <- ExprGuiM.withLocalPrecedence 0 (const (Prec.make 0)) mkFuncRow
        relayedArgs <-
            case apply ^. Sugar.aRelayedArgs of
            [] -> return []
            args -> mkRelayedArgs nearestHoles args <&> (:[])
        ExpressionGui.addValFrame
            <*> (Responsive.vboxSpaced ?? (funcRow : argRows ++ relayedArgs))

makeSimple ::
    Monad m =>
    Sugar.Apply (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeSimple (Sugar.Apply func arg) pl =
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let mParensId
                | Prec.needParens parentPrec (Prec.my prefixPrecedence) =
                    Just (Widget.toAnimId myId)
                | otherwise = Nothing
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
            <*> sequenceA
            [ ExprGuiM.makeSubexpressionWith
              0 (ExpressionGui.after .~ prefixPrecedence+1) func
            , ExprGuiM.makeSubexpressionWith
              prefixPrecedence (ExpressionGui.before .~ prefixPrecedence) arg
            ]
    & ExpressionGui.stdWrapParentExpr pl (func ^. Sugar.rPayload . Sugar.plEntityId)
    where
        myId = WidgetIds.fromExprPayload pl
