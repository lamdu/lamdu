{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.Lens ((^.))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Control.MonadA (MonadA)
import qualified Control.Lens as Lens
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data as Data
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

-- make without the focus delegator
makeInner ::
  MonadA m => Sugar.Polymorphic (m ()) (Sugar.Expression m) -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeInner poly myId = do
  -- TODO: This is just to detect whether cursor is in the full
  -- expression.  Even when it's not displayed, which may be wasteful
  -- (even with laziness, at least the names are going to be read).
  fullExprEdit <- ExprGuiM.makeSubexpresion $ Sugar.pFullExpression poly
  -- We are inside a focus delegator made by 'make' below, so if the
  -- cursor is on us it means user entered our widget.
  if fullExprEdit ^. ExpressionGui.egWidget . Widget.wIsFocused
    then
      return $ bg Layers.expandedPolymorphicBG Config.polymorphicFullBGColor fullExprEdit
    else
      colorize (Sugar.pCompact poly) $
      VarEdit.makeView (Sugar.pCompact poly) funcId
  where
    funcId = WidgetIds.fromGuid $ Sugar.pFuncGuid poly
    colorize (Data.ParameterRef _) =
      (fmap . bg Layers.compactPolymorphicBG) Config.polymorphicCompactBGColor .
      ExprGuiM.atEnv (WE.setTextColor Config.parameterColor)
    colorize (Data.DefinitionRef _) =
      ExprGuiM.atEnv (WE.setTextColor Config.polymorphicForegroundColor)
    bg layer =
      Lens.over ExpressionGui.egWidget .
      Widget.backgroundColor layer (Widget.toAnimId myId ++ ["bg"])

polymorphicFDConfig :: FocusDelegator.Config
polymorphicFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.expandPolymorphicKey
  , FocusDelegator.startDelegatingDoc = "Expand polymorphic"
  , FocusDelegator.stopDelegatingKey = Config.collapsePolymorphicKey
  , FocusDelegator.stopDelegatingDoc = "Collapse polymorphic"
  }

make ::
  MonadA m => Sugar.Polymorphic (m ()) (Sugar.Expression m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make poly =
  ExpressionGui.wrapDelegated polymorphicFDConfig FocusDelegator.NotDelegating $
  makeInner poly
