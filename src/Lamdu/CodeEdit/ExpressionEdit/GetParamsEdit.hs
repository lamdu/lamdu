module Lamdu.CodeEdit.ExpressionEdit.GetParamsEdit
  ( make
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar.Types as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

make
  :: MonadA m
  => Sugar.GetParams Sugar.Name m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getParams myId = do
  cp <- ExprGuiM.readCodeAnchors
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
      (E.Doc ["Navigation", "Jump to definition"]) $ do
        DataOps.savePreJumpPosition cp myId
        WidgetIds.fromGuid <$> getParams ^. Sugar.gpJumpTo
  paramsLabel <-
    ExprGuiM.withFgColor (Config.parameterColor config) $ label "params"
  prefixLabel <- label "(of "
  defNameLabel <-
    ExprGuiM.withFgColor (Config.definitionColor config) .
    ExprGuiM.widgetEnv $ ExpressionGui.makeNameView defName animId
  suffixLabel <- label ")"
  ExprGuiM.widgetEnv .
    fmap ExpressionGui.fromValueWidget .
    BWidgets.makeFocusableView myId .
    Widget.weakerEvents jumpToDefinitionEventMap $
    BWidgets.hboxCenteredSpaced
    [ paramsLabel
    , Widget.scale (realToFrac <$> Config.paramDefSuffixScaleFactor config) $
      Box.hboxCentered [prefixLabel, defNameLabel, suffixLabel]
    ]
  where
    animId = Widget.toAnimId myId
    label = ExprGuiM.widgetEnv . flip BWidgets.makeLabel animId
    defName = getParams ^. Sugar.gpDefName
