module Lamdu.CodeEdit.ExpressionEdit.GetVarEdit
  ( make, makeUncoloredView, makeView
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Config (Config)
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

makeUncoloredView
  :: MonadA m
  => Sugar.GetVar Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeUncoloredView getVar myId =
  ExprGuiM.widgetEnv $
  fmap ExpressionGui.fromValueWidget .
  BWidgets.makeFocusableView myId =<<
  ExpressionGui.makeNameView (getVar ^. Sugar.gvName) (Widget.toAnimId myId)

colorOf :: Config -> Sugar.GetVarType -> Draw.Color
colorOf config Sugar.GetDefinition = Config.definitionColor config
colorOf config Sugar.GetParameter = Config.parameterColor config
colorOf config Sugar.GetFieldParameter = Config.parameterColor config

makeView
  :: MonadA m
  => Sugar.GetVar Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeView getParam myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  (ExprGuiM.withFgColor . colorOf config) (getParam ^. Sugar.gvVarType) $
    makeUncoloredView getParam myId

make
  :: MonadA m
  => Sugar.GetVar Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getVar myId = do
  ExprGuiM.markVariablesAsUsed [getVar ^. Sugar.gvIdentifier]
  cp <- ExprGuiM.readCodeAnchors
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
      (E.Doc ["Navigation", "Jump to definition"]) $ do
        DataOps.savePreJumpPosition cp myId
        WidgetIds.fromGuid <$> getVar ^. Sugar.gvJumpTo
  makeView getVar myId &
    Lens.mapped . ExpressionGui.egWidget %~
    Widget.weakerEvents jumpToDefinitionEventMap
