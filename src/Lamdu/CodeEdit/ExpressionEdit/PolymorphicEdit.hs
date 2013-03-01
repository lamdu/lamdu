{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, Collapser(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expression as Expression
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
  MonadA m => Sugar.Polymorphic (Tag m) (Sugar.Expression m) ->
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
        ExprGuiM.makeSubexpresion $ Sugar.pFullExpression poly
      , cMakeFocusedCompact =
        colorize bgId (Sugar.pCompact poly) $
        VarEdit.makeView (Sugar.pCompact poly) funcId
      }
      where
        bgId = Widget.toAnimId myId ++ ["bg"]
    colorize bgId (Expression.ParameterRef _) =
      fmap
      (ExpressionGui.withBgColor
       Layers.polymorphicCompactBG
       Config.polymorphicCompactBGColor bgId) .
      ExprGuiM.withFgColor Config.parameterColor
    colorize _ (Expression.DefinitionRef _) =
      ExprGuiM.withFgColor Config.polymorphicForegroundColor
    funcId = WidgetIds.fromGuid $ Sugar.pFuncGuid poly
