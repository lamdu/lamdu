{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( make
    ) where

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
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Names.Get as NamesGet
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

prefixPrecedence :: Int
prefixPrecedence = 10

mkOverrideModifyEventMap ::
    Monad m => Sugar.Actions m ->
    ExprGuiM m (ExpressionGui m -> ExpressionGui m)
mkOverrideModifyEventMap actions =
    do
        config <- ExprGuiM.readConfig
        TreeLayout.widget %~
            Widget.strongerEvents (ExprEventMap.modifyEventMap config actions)
            & return

mkPrecedence :: Monad m => Sugar.Apply name (ExprGuiT.SugarExpr m) -> Int
mkPrecedence (Sugar.Apply func specialArgs _annotatedArgs) =
    case specialArgs of
    Sugar.NoSpecialArgs -> 0
    Sugar.ObjectArg{} -> prefixPrecedence
    Sugar.InfixArgs _ _ ->
        case NamesGet.fromExpression func <&> Text.unpack . nName of
        [x:_] -> CharClassification.charPrecedence x
        _ -> 20

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
    Monad m => ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeInfixFuncName func =
    do
        res <- ExprGuiM.makeSubexpressionWith (const prec) func
        if any BinderEdit.nonOperatorName (NamesGet.fromExpression func)
            then
                res
                & TreeLayout.widget %~
                    addInfixMarker (WidgetIds.fromExprPayload (func ^. Sugar.rPayload))
                & return
            else return res
    where
        -- TODO: What precedence to give when it must be atomic?:
        prec = Prec.make 20

makeFuncRow ::
    Monad m =>
    Maybe AnimId ->
    Int ->
    Sugar.Apply name (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m a -> ExprGuiM m (ExpressionGui m)
makeFuncRow mParensId prec (Sugar.Apply func specialArgs annotatedArgs) pl =
    do
        overrideModifyEventMap <- mkOverrideModifyEventMap (pl ^. Sugar.plActions)
        case specialArgs of
            Sugar.NoSpecialArgs ->
                ExprGuiM.makeSubexpressionWith (const (Prec.make prec)) func
                <&> overrideModifyEventMap
            Sugar.ObjectArg arg ->
                ExpressionGui.combineSpaced mParensId
                <*> sequenceA
                [ ExprGuiM.makeSubexpressionWith
                  (ExpressionGui.after .~ prec+1) func
                  <&> maybeOverrideModifyEventMap
                , ExprGuiM.makeSubexpressionWith
                  (ExpressionGui.before .~ prec) arg
                ]
                where
                    maybeOverrideModifyEventMap
                        | null annotatedArgs = id
                        | otherwise = overrideModifyEventMap
            Sugar.InfixArgs l r ->
                ExpressionGui.combineSpaced mParensId
                <*> sequenceA
                [ ExpressionGui.combineSpaced Nothing
                    <*> sequenceA
                    [ ExprGuiM.makeSubexpressionWith (ExpressionGui.after .~ prec) l
                    , makeInfixFuncName func <&> overrideModifyEventMap
                    ]
                , ExprGuiM.makeSubexpressionWith (ExpressionGui.before .~ prec+1) r
                ]

make ::
    Monad m =>
    Sugar.Apply (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make apply@(Sugar.Apply func _specialArgs annotatedArgs) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let needParens =
                not isBoxed
                && Prec.needParens parentPrec (Prec.my prec)
        let mParensId
                | needParens = Just (Widget.toAnimId myId)
                | otherwise = Nothing
        makeFuncRow mParensId prec apply pl
            & ( if needParens
                then ExprGuiM.withLocalPrecedence (const (Prec.make 0))
                else
                if isBoxed
                then mkBoxed annotatedArgs myId
                else id
              )
            & ExprGuiM.assignCursor myId funcId
    where
        funcId = func ^. Sugar.rPayload & WidgetIds.fromExprPayload
        isBoxed = not $ null annotatedArgs
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
        funcRow <- ExprGuiM.withLocalPrecedence (const (Prec.make 0)) mkFuncRow
        vbox <- ExpressionGui.vboxTopFocalSpaced
        ExpressionGui.addValFrame myId
            ?? vbox
                ([funcRow, vbox argRows] <&> TreeLayout.alignment . _1 .~ 0)
