{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CollapsedEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui, Collapser(..), ParentPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

collapsedFDConfig :: Config -> FocusDelegator.Config
collapsedFDConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.collapsedExpandKeys config
  , FocusDelegator.startDelegatingDoc = E.Doc ["View", "Expand application"]
  , FocusDelegator.stopDelegatingKeys = Config.collapsedCollapseKeys config
  , FocusDelegator.stopDelegatingDoc = E.Doc ["View", "Collapse application"]
  }

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Collapsed Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) collapsed myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    makeExpanded wId =
      ExpressionGui.withBgColor
      (Config.layerCollapsedExpandedBG (Config.layers config))
      (Config.collapsedExpandedBGColor config) (bgId wId) <$>
      ExprGuiM.makeSubexpression parentPrecedence fullExpression
    f wId =
      Collapser
      { cMakeExpanded = makeExpanded wId
      , cMakeFocusedCompact =
        colorize wId (compact ^. Sugar.gvVarType) $
        GetVarEdit.makeUncoloredView compact funcId
      }
    colorize _ Sugar.GetDefinition =
      ExprGuiM.withFgColor $ Config.collapsedForegroundColor config
    colorize wId _ = colorizeGetParameter wId
    colorizeGetParameter wId =
      fmap
      (ExpressionGui.withBgColor
       (Config.layerCollapsedCompactBG (Config.layers config))
       (Config.collapsedCompactBGColor config) (bgId wId)) .
      ExprGuiM.withFgColor (Config.parameterColor config)
  case () of
    _ | hasInfo -> makeExpanded myId
      | otherwise -> ExpressionGui.makeCollapser (collapsedFDConfig config) f myId
  where
    Sugar.Collapsed funcGuid compact fullExpression hasInfo = collapsed
    bgId wId = Widget.toAnimId wId ++ ["bg"]
    funcId = WidgetIds.fromGuid funcGuid
