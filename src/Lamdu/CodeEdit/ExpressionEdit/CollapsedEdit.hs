{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.CollapsedEdit(make) where

import Control.Applicative ((<$>))
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
make (ParentPrecedence parentPrecedence) collapsed
  | hasInfo = makeExpanded
  | otherwise = ExpressionGui.makeCollapser collapsedFDConfig f
  where
    Sugar.Collapsed funcGuid compact fullExpression hasInfo = collapsed
    makeExpanded myId =
      ExpressionGui.withBgColor Layers.collapsedExpandedBG
      Config.collapsedExpandedBGColor (bgId myId) <$>
      ExprGuiM.makeSubexpresion parentPrecedence fullExpression
    f myId =
      Collapser
      { cMakeExpanded = makeExpanded myId
      , cMakeFocusedCompact =
        colorize myId (compact ^. Sugar.gvVarType) $
        GetVarEdit.makeUncoloredView compact funcId
      }
    bgId myId = Widget.toAnimId myId ++ ["bg"]
    funcId = WidgetIds.fromGuid funcGuid
    colorize _ Sugar.GetDefinition =
      ExprGuiM.withFgColor Config.collapsedForegroundColor
    colorize myId _ = colorizeGetParameter myId
    colorizeGetParameter myId =
      fmap
      (ExpressionGui.withBgColor
       Layers.collapsedCompactBG
       Config.collapsedCompactBGColor (bgId myId)) .
      ExprGuiM.withFgColor Config.parameterColor
