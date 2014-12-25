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
  makeView getVar myId
    & ExpressionGui.stdWrap pl
    <&> ExpressionGui.egWidget %~ Widget.weakerEvents jumpToDefinitionEventMap
