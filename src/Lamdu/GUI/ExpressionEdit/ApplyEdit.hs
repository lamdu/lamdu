{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( makeSimple, makeLabeled, prefixPrecedence
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View, (/|/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
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
import qualified Lamdu.GUI.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
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
        x:_ -> CharClassification.charPrecedence x
        _ -> 20
    where
        funcName =
            apply ^. Sugar.aFunc . Sugar.bvNameRef . Sugar.nrName
            & nName & Text.unpack

infixMarker :: Vector2 Anim.R -> Draw.Image ()
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

addInfixMarker :: View.SetLayers a => Widget.Id -> a -> a
addInfixMarker widgetId =
    View.setLayers %@~
    \size ->
    View.bottomLayer <>~ Anim.simpleFrame frameId (infixMarker size)
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
            | funcVar ^. Sugar.bvNameRef . Sugar.nrName & BinderEdit.nonOperatorName =
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
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeFuncRow mParensId prec apply myId =
    case apply ^. Sugar.aSpecialArgs of
    Sugar.NoSpecialArgs ->
        case apply ^. Sugar.aAnnotatedArgs of
        [] -> error "apply with no args!"
        (x:_) ->
            makeFuncVar (ExprGuiT.nextHolesBefore (x ^. Sugar.aaExpr)) funcVar
            myId <&> TreeLayout.fromWithTextPos
    Sugar.ObjectArg arg ->
        ExpressionGui.combineSpacedMParens mParensId
        <*> sequenceA
        [ makeFuncVar (ExprGuiT.nextHolesBefore arg) funcVar myId
            <&> TreeLayout.fromWithTextPos
        , ExprGuiM.makeSubexpressionWith
          (if isBoxed apply then 0 else prec)
          (ExpressionGui.before .~ prec) arg
        ]
    Sugar.InfixArgs l r ->
        ExpressionGui.combineSpacedMParens mParensId
        <*> sequenceA
        [ ExpressionGui.combineSpaced
            <*> sequenceA
            [ ExprGuiM.makeSubexpressionWith 0 (ExpressionGui.after .~ prec) l
            , makeInfixFuncName (ExprGuiT.nextHolesBefore r) funcVar myId
                <&> TreeLayout.fromWithTextPos
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
        makeFuncRow mParensId prec apply myId
            & addBox
            & ExpressionGui.stdWrapParentExpr pl
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
        paramTag <- TagEdit.makeParamTag (arg ^. Sugar.aaTag)
        space <- Spacer.stdHSpace
        expr <- ExprGuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        return (paramTag /|/ space, expr)

mkRelayedArgs :: Monad m => NearestHoles -> [Sugar.RelayedArg (Name m) m] -> ExprGuiM m (ExpressionGui m)
mkRelayedArgs nearestHoles args =
    do
        argEdits <- mapM makeArgEdit args
        collapsed <- ExpressionGui.grammarLabel "➾" <&> TreeLayout.fromTextView
        ExpressionGui.combineSpaced ?? collapsed : argEdits
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
                    <&> TreeLayout.fromWithTextPos
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
                TreeLayout.taggedList
                <*> (traverse makeArgRow xs <&> Lens.mapped . _1 . Align.tValue %~ Widget.fromView) <&> (:[])
        funcRow <- ExprGuiM.withLocalPrecedence 0 (const (Prec.make 0)) mkFuncRow
        relayedArgs <-
            case apply ^. Sugar.aRelayedArgs of
            [] -> return []
            args -> mkRelayedArgs nearestHoles args <&> (:[])
        ExpressionGui.addValFrame
            <*> (TreeLayout.vboxSpaced ?? (funcRow : argRows ++ relayedArgs))

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
        ExpressionGui.combineSpacedMParens mParensId
            <*> sequenceA
            [ ExprGuiM.makeSubexpressionWith
              0 (ExpressionGui.after .~ prefixPrecedence+1) func
            , ExprGuiM.makeSubexpressionWith
              prefixPrecedence (ExpressionGui.before .~ prefixPrecedence) arg
            ]
    & Widget.assignCursor myId (func ^. Sugar.rPayload & WidgetIds.fromExprPayload)
    & ExpressionGui.stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
