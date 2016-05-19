{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Parens (parenify)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.Precedence (MyPrecedence(..))
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Names.Get as NamesGet
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

prefixPrecedence :: Int
prefixPrecedence = 10

mkOverrideModifyEventMap ::
    Monad m => Sugar.Actions m ->
    ExprGuiM m (ExpressionGui m -> ExpressionGui m)
mkOverrideModifyEventMap actions =
    do
        config <- ExprGuiM.readConfig
        ExpressionGui.egWidget %~
            Widget.strongerEvents (ExprEventMap.modifyEventMap [] config actions)
            & return

mkPrecedence :: Sugar.SpecialArgs a -> MyPrecedence
mkPrecedence specialArgs =
    case specialArgs of
    Sugar.NoSpecialArgs -> 0
    Sugar.ObjectArg{} -> prefixPrecedence
    Sugar.InfixArgs prec _ _ -> prec
    & fromIntegral & MyPrecedence

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
    & Widget.animFrame
    <>~ Anim.simpleFrame frameId (infixMarker (widget ^. Widget.size))
    where
        frameId = Widget.toAnimId widgetId ++ ["infix"]

makeInfixFuncName ::
    Monad m => ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeInfixFuncName func =
    do
        res <- ExprGuiM.makeSubexpression (const prec) func
        if any BinderEdit.nonOperatorName (NamesGet.fromExpression func)
            then
                res
                & ExpressionGui.egWidget %~
                    addInfixMarker (WidgetIds.fromExprPayload (func ^. Sugar.rPayload))
                & return
            else return res
    where
        -- TODO: What precedence to give when it must be atomic?:
        prec = 20

makeFuncRow ::
    Monad m =>
    Sugar.Apply name (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m a -> ExprGuiM m (ExpressionGui m)
makeFuncRow (Sugar.Apply func specialArgs annotatedArgs) pl =
    do
        overrideModifyEventMap <- mkOverrideModifyEventMap (pl ^. Sugar.plActions)
        case specialArgs of
            Sugar.NoSpecialArgs ->
                ExprGuiM.makeSubexpression (const 0) func
                <&> overrideModifyEventMap
            Sugar.ObjectArg arg ->
                ExpressionGui.combineSpaced
                <*> sequenceA
                [ ExprGuiM.makeSubexpression
                  (ExpressionGui.precRight .~ prefixPrecedence+1) func
                  <&> maybeOverrideModifyEventMap
                , ExprGuiM.makeSubexpression
                  (ExpressionGui.precLeft .~ prefixPrecedence) arg
                ]
                where
                    maybeOverrideModifyEventMap
                        | null annotatedArgs = id
                        | otherwise = overrideModifyEventMap
            Sugar.InfixArgs prec l r ->
                ExpressionGui.combineSpaced
                <*> sequenceA
                [ ExpressionGui.combineSpaced
                    <*> sequenceA
                    [ ExprGuiM.makeSubexpression (ExpressionGui.precRight .~ prec+1) l
                    , makeInfixFuncName func <&> overrideModifyEventMap
                    ]
                , ExprGuiM.makeSubexpression (ExpressionGui.precLeft .~ prec+1) r
                ]

make ::
    Monad m =>
    Sugar.Apply (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make apply@(Sugar.Apply func specialArgs annotatedArgs) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    makeFuncRow apply pl
    & ( if isBoxed
        then mkBoxed annotatedArgs myId
        else parenify myPrecedence myId
      )
    & ExprGuiM.assignCursor myId funcId
    where
        funcId = func ^. Sugar.rPayload & WidgetIds.fromExprPayload
        myPrecedence = mkPrecedence specialArgs
        isBoxed = not $ null annotatedArgs

makeArgRows ::
    Monad m =>
    Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeArgRows arg =
    ExpressionGui.tagItem
    <*> TagEdit.makeParamTag (arg ^. Sugar.aaTag)
    <*> ExprGuiM.makeSubexpression (const 0) (arg ^. Sugar.aaExpr)

mkBoxed ::
    Monad m =>
    [Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m)] ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
mkBoxed annotatedArgs myId mkFuncRow =
    do
        argRows <- traverse makeArgRows annotatedArgs
        funcRow <- ExprGuiM.withLocalPrecedence (const 0) mkFuncRow
        vbox <- ExpressionGui.vboxTopFocalSpaced
        ExpressionGui.addValFrame myId
            ?? vbox
                ([funcRow, vbox argRows] <&> ExpressionGui.egAlignment . _1 .~ 0)
