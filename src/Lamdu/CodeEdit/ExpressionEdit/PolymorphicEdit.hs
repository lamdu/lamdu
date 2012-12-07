{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, Collapser(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data as Data
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetIds as WidgetIds

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
  ExpressionGui.makeCollapser polymorphicFDConfig f
  where
    f myId =
      Collapser
      { cMakeExpanded = ExprGuiM.makeSubexpresion $ Sugar.pFullExpression poly
      , cOnFocusedExpanded =
        ExpressionGui.withBgColor
        Layers.polymorphicExpandedBG
        Config.polymorphicExpandedBGColor bgId
      , cMakeFocusedCompact =
        colorize bgId (Sugar.pCompact poly) $
        VarEdit.makeView (Sugar.pCompact poly) funcId
      }
      where
        bgId = Widget.toAnimId myId ++ ["bg"]
    colorize bgId (Data.ParameterRef _) =
      fmap
      (ExpressionGui.withBgColor
       Layers.polymorphicCompactBG
       Config.polymorphicCompactBGColor bgId) .
      ExprGuiM.withFgColor Config.parameterColor
    colorize _ (Data.DefinitionRef _) =
      ExprGuiM.withFgColor Config.polymorphicForegroundColor
    funcId = WidgetIds.fromGuid $ Sugar.pFuncGuid poly
