{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.CollapsedEdit(make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, Collapser(..), ParentPrecedence(..))
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

collapsedFDConfig :: FocusDelegator.Config
collapsedFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.collapsedExpandKeys
  , FocusDelegator.startDelegatingDoc = E.Doc ["View", "Expand application"]
  , FocusDelegator.stopDelegatingKeys = Config.collapsedCollapseKeys
  , FocusDelegator.stopDelegatingDoc = E.Doc ["View", "Collapse application"]
  }

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Collapsed Sugar.Name m (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) poly =
  ExpressionGui.makeCollapser collapsedFDConfig f
  where
    f myId =
      Collapser
      { cMakeExpanded =
        fmap
        (ExpressionGui.withBgColor Layers.collapsedExpandedBG
         Config.collapsedExpandedBGColor bgId) .
        ExprGuiM.makeSubexpresion parentPrecedence $ poly ^. Sugar.cFullExpression
      , cMakeFocusedCompact =
        colorize bgId (poly ^. Sugar.cCompact . Sugar.gvVarType) $
        GetVarEdit.makeUncoloredView (poly ^. Sugar.cCompact) funcId
      }
      where
        bgId = Widget.toAnimId myId ++ ["bg"]
    funcId = WidgetIds.fromGuid $ poly ^. Sugar.cFuncGuid
    colorize _ Sugar.GetDefinition =
      ExprGuiM.withFgColor Config.collapsedForegroundColor
    colorize bgId _ = colorizeGetParameter bgId
    colorizeGetParameter bgId =
      fmap
      (ExpressionGui.withBgColor
       Layers.collapsedCompactBG
       Config.collapsedCompactBGColor bgId) .
      ExprGuiM.withFgColor Config.parameterColor
