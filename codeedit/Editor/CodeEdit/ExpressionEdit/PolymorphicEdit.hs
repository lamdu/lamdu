{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ExprGuiM)
import Editor.MonadF (MonadF)
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Layers as Layers
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

-- make without the focus delegator
makeInner ::
  MonadF m => Sugar.Polymorphic (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeInner poly myId = do
  -- TODO: This is just to detect whether cursor is in the full expression.
  -- Even when it's not displayed, which is wasteful.
  fullExprEdit <- ExpressionGui.makeSubexpresion $ Sugar.pFullExpression poly
  -- We are inside a non-delegating focus delegator made by makeSubexpresion,
  -- so if the cursor is on us it means user enterred our widget.
  if Widget.wIsFocused (ExpressionGui.egWidget fullExprEdit)
    then
      return $ bg Layers.expandedPolymorphicBG Config.polymorphicFullBGColor fullExprEdit
    else
      colorize (Sugar.pCompact poly) $
      VarEdit.makeView (Sugar.pCompact poly) funcId
  where
    funcId = WidgetIds.fromGuid $ Sugar.pFuncGuid poly
    colorize (Sugar.GetParameter _) =
      (liftM . bg Layers.compactPolymorphicBG) Config.polymorphicCompactBGColor .
      ExprGuiM.atEnv (OT.setTextColor Config.parameterColor)
    colorize (Sugar.GetDefinition _) =
      ExprGuiM.atEnv (OT.setTextColor Config.polymorphicForegroundColor)
    bg layer =
      ExpressionGui.atEgWidget .
      Widget.backgroundColor layer (Widget.toAnimId myId ++ ["bg"])

polymorphicFDConfig :: FocusDelegator.Config
polymorphicFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.expandPolymorphicKey
  , FocusDelegator.startDelegatingDoc = "Expand polymorphic"
  , FocusDelegator.stopDelegatingKey = Config.collapsePolymorphicKey
  , FocusDelegator.stopDelegatingDoc = "Collapse polymorphic"
  }

make ::
  MonadF m => Sugar.Polymorphic (Sugar.Expression m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make poly =
  ExpressionGui.wrapDelegated polymorphicFDConfig FocusDelegator.NotDelegating
  ExpressionGui.atEgWidget $ makeInner poly
