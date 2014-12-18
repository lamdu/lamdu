module Lamdu.GUI.ExpressionEdit.GetVarEdit
  ( make
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

makeSimpleView ::
  MonadA m =>
  Draw.Color ->
  Sugar.GetVar (Name m) m ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeSimpleView color getVar myId =
  ExpressionGui.makeNameView (getVar ^. Sugar.gvName) (Widget.toAnimId myId)
  >>= BWidgets.makeFocusableView myId
  <&> ExpressionGui.fromValueWidget
  & ExprGuiM.widgetEnv
  & ExprGuiM.withFgColor color

makeGetParamsView ::
  MonadA m =>
  Sugar.GetVar (Name m) m -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeGetParamsView getParams myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  paramsLabel <-
    ExprGuiM.withFgColor (Config.parameterColor config) $ label "params"
  prefixLabel <- label "(of "
  defNameLabel <-
    ExprGuiM.withFgColor (Config.definitionColor config) .
    ExprGuiM.widgetEnv $ ExpressionGui.makeNameView defName animId
  suffixLabel <- label ")"
  let
    hbox =
      BWidgets.hboxCenteredSpaced
      [ paramsLabel
      , Widget.scale (realToFrac <$> Config.paramDefSuffixScaleFactor config) $
        Box.hboxCentered [prefixLabel, defNameLabel, suffixLabel]
      ]
  hbox
    & BWidgets.makeFocusableView myId
    <&> ExpressionGui.fromValueWidget
    & ExprGuiM.widgetEnv
  where
    animId = Widget.toAnimId myId
    label = ExprGuiM.widgetEnv . flip BWidgets.makeLabel animId
    defName = getParams ^. Sugar.gvName

make ::
  MonadA m =>
  Sugar.GetVar (Name m) m ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make getVar pl myId = do
  cp <- ExprGuiM.readCodeAnchors
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
      (E.Doc ["Navigation", "Jump to definition"]) $ do
        DataOps.savePreJumpPosition cp myId
        WidgetIds.fromEntityId <$> getVar ^. Sugar.gvJumpTo
    makeView =
      case getVar ^. Sugar.gvVarType of
      Sugar.GetDefinition -> makeSimpleView (Config.definitionColor config)
      Sugar.GetParameter -> makeSimpleView (Config.parameterColor config)
      Sugar.GetFieldParameter -> makeSimpleView (Config.parameterColor config)
      Sugar.GetParamsRecord -> makeGetParamsView
  makeView getVar myId
    & ExpressionGui.stdWrap pl
    <&> ExpressionGui.egWidget %~ Widget.weakerEvents jumpToDefinitionEventMap
