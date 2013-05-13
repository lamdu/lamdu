{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.CollapsedEdit(make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, Collapser(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetIds as WidgetIds

polymorphicFDConfig :: FocusDelegator.Config
polymorphicFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.polymorphicExpandKey
  , FocusDelegator.startDelegatingDoc = E.Doc ["View", "Expand polymorphic"]
  , FocusDelegator.stopDelegatingKey = Config.polymorphicCollapseKey
  , FocusDelegator.stopDelegatingDoc = E.Doc ["View", "Collapse polymorphic"]
  }

make ::
  MonadA m => Sugar.Collapsed Sugar.Name m (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make poly =
  ExpressionGui.makeCollapser polymorphicFDConfig f
  where
    f myId =
      Collapser
      { cMakeExpanded =
        fmap
        (ExpressionGui.withBgColor Layers.polymorphicExpandedBG
         Config.polymorphicExpandedBGColor bgId) .
        ExprGuiM.makeSubexpresion $ poly ^. Sugar.pFullExpression
      , cMakeFocusedCompact =
        colorize bgId (poly ^. Sugar.pCompact . Sugar.gvVarType) $
        GetVarEdit.makeUncoloredView (poly ^. Sugar.pCompact) funcId
      }
      where
        bgId = Widget.toAnimId myId ++ ["bg"]
    funcId = WidgetIds.fromGuid $ poly ^. Sugar.pFuncGuid
    colorize bgId Sugar.GetParameter = colorizeGetParameter bgId
    colorize bgId Sugar.GetFieldParameter = colorizeGetParameter bgId
    colorize _ Sugar.GetDefinition =
      ExprGuiM.withFgColor Config.polymorphicForegroundColor
    colorizeGetParameter bgId =
      fmap
      (ExpressionGui.withBgColor
       Layers.polymorphicCompactBG
       Config.polymorphicCompactBGColor bgId) .
      ExprGuiM.withFgColor Config.parameterColor
