{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( makeSimple, makeLabeled, prefixPrecedence
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.CharClassification as CharClassification
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
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
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

prefixPrecedence :: Int
prefixPrecedence = 10

mkPrecedence :: Sugar.LabeledApply n0 (Sugar.BinderVar (Name m) m0) expr -> Int
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

addInfixMarker :: Widget.Id -> Widget a -> Widget a
addInfixMarker widgetId widget =
    widget
    & Widget.bottomFrame
    <>~ Anim.simpleFrame frameId (infixMarker (widget ^. Widget.size))
    where
        frameId = Widget.toAnimId widgetId ++ ["infix"]

makeInfixFuncName ::
    Monad m => Sugar.BinderVar (Name m) m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInfixFuncName funcVar myId =
    GetVarEdit.makeGetBinder funcVar myId
    <&> mAddMarker
    where
        mAddMarker
            | funcVar ^. Sugar.bvNameRef . Sugar.nrName & BinderEdit.nonOperatorName =
                TreeLayout.widget %~ addInfixMarker myId
            | otherwise = id

isBoxed :: Sugar.LabeledApply name binderVar a -> Bool
isBoxed = Lens.has (Sugar.aAnnotatedArgs . Lens.traversed)

makeFuncRow ::
    Monad m =>
    Maybe AnimId ->
    Int ->
    Sugar.LabeledApply name (Sugar.BinderVar (Name m) m) (ExprGuiT.SugarExpr m) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeFuncRow mParensId prec apply myId =
    case specialArgs of
    Sugar.NoSpecialArgs -> GetVarEdit.makeGetBinder funcVar myId
    Sugar.ObjectArg arg ->
        ExpressionGui.combineSpacedMParens mParensId
        <*> sequenceA
        [ GetVarEdit.makeGetBinder funcVar myId
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
            , makeInfixFuncName funcVar myId
            ]
        , ExprGuiM.makeSubexpressionWith (prec+1) (ExpressionGui.before .~ prec+1) r
        ]
    where
        Sugar.LabeledApply funcVar specialArgs _ = apply

makeLabeled ::
    Monad m =>
    Sugar.LabeledApply (Name m) (Sugar.BinderVar (Name m) m) (ExprGuiT.SugarExpr m) ->
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
        fixPrec . ExpressionGui.stdWrapParentExpr pl $
            \myId ->
            do
                let mParensId
                        | needParens = Just (Widget.toAnimId myId)
                        | otherwise = Nothing
                makeFuncRow mParensId prec apply myId
                    &
                    if isBoxed apply
                    then mkBoxed (apply ^. Sugar.aAnnotatedArgs) myId
                    else id
    where
        prec = mkPrecedence apply

makeArgRows ::
    Monad m =>
    Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeArgRows arg =
    ExpressionGui.tagItem
    <*> TagEdit.makeParamTag (arg ^. Sugar.aaTag)
    <*> ExprGuiM.makeSubexpression (arg ^. Sugar.aaExpr)

mkBoxed ::
    Monad m =>
    [Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m)] ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
mkBoxed annotatedArgs myId mkFuncRow =
    do
        argRows <- traverse makeArgRows annotatedArgs
        funcRow <- ExprGuiM.withLocalPrecedence 0 (const (Prec.make 0)) mkFuncRow
        vbox <- ExpressionGui.vboxTopFocalSpaced
        ExpressionGui.addValFrame myId
            ?? vbox
                ([funcRow, vbox argRows] <&> TreeLayout.alignment . _1 .~ 0)

makeSimple ::
    Monad m =>
    Sugar.Apply (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeSimple (Sugar.Apply func arg) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
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
    & ExprGuiM.assignCursor myId (func ^. Sugar.rPayload & WidgetIds.fromExprPayload)
