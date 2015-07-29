{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
    ( make
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui, MyPrecedence(..), ParentPrecedence(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

mkOverrideModifyEventMap ::
    MonadA m => Maybe (Sugar.Actions m) ->
    ExprGuiM m (ExpressionGui m -> ExpressionGui m)
mkOverrideModifyEventMap Nothing = return id
mkOverrideModifyEventMap (Just actions) =
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
    Sugar.InfixArgs{} -> infixPrecedence
    & MyPrecedence

makeFuncRow ::
    MonadA m =>
    Sugar.Apply name (ExprGuiT.SugarExpr m) -> Sugar.Payload m a ->
    ExprGuiM m (ExpressionGui m)
makeFuncRow (Sugar.Apply func specialArgs annotatedArgs) pl =
    do
        overrideModifyEventMap <- mkOverrideModifyEventMap (pl ^. Sugar.plActions)
        case specialArgs of
            Sugar.NoSpecialArgs ->
                ExprGuiM.makeSubexpression 0 func
                <&> overrideModifyEventMap
            Sugar.ObjectArg arg ->
                sequenceA
                [ ExprGuiM.makeSubexpression (prefixPrecedence+1) func
                    <&> if null annotatedArgs
                        then id
                        else overrideModifyEventMap
                , ExprGuiM.makeSubexpression
                    (if isBoxed then 0 else prefixPrecedence)
                    arg
                ]
                >>= ExpressionGui.hboxSpaced
            Sugar.InfixArgs l r ->
                sequenceA
                [ ExprGuiM.makeSubexpression (infixPrecedence+1) l
                , -- TODO: What precedence to give when it must be atomic?:
                    ExprGuiM.makeSubexpression 20 func <&> overrideModifyEventMap
                , ExprGuiM.makeSubexpression (infixPrecedence+1) r
                ]
                >>= ExpressionGui.hboxSpaced
    where
        isBoxed = not $ null annotatedArgs

make ::
    MonadA m => ParentPrecedence ->
    Sugar.Apply (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make parentPrecedence apply@(Sugar.Apply func specialArgs annotatedArgs) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        funcRow <- makeFuncRow apply pl
        if isBoxed
            then mkBoxed funcRow annotatedArgs myId
            else
                ExpressionGui.parenify parentPrecedence
                (mkPrecedence specialArgs) myId funcRow
    & ExprGuiM.assignCursor myId funcId
    where
        funcId = func ^. Sugar.rPayload & WidgetIds.fromExprPayload
        isBoxed = not $ null annotatedArgs

makeArgRows ::
    MonadA m =>
    Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m [[(Grid.Alignment, Widget (T m))]]
makeArgRows arg =
    do
        argTagEdit <- TagEdit.makeParamTag (arg ^. Sugar.aaTag)
        argValEdit <- ExprGuiM.makeSubexpression 0 $ arg ^. Sugar.aaExpr
        vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
        space <- ExprGuiM.widgetEnv BWidgets.stdSpaceWidget
        pure
            [ replicate 3 (0.5, vspace)
            , [ argTagEdit ^. Layout.alignedWidget & _1 . _1 .~ 0
                , (0.5, space)
                , argValEdit ^. Layout.alignedWidget & _1 . _1 .~ 0
                ]
            ]

mkBoxed ::
    MonadA m =>
    ExpressionGui m ->
    [Sugar.AnnotatedArg (Name m) (ExprGuiT.SugarExpr m)] ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
mkBoxed funcRow annotatedArgs myId =
    do
        grid <-
            annotatedArgs
            & traverse makeArgRows
            <&> Grid.toWidget . Grid.make . concat
        funcRow
            & ExpressionGui.addBelow 0 [(0, grid)]
            & ExpressionGui.addValFrame myId
