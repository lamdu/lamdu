module Lamdu.GUI.ExpressionEdit.AtomEdit(make) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.Sugar.Types as Sugar

make ::
	MonadA m =>
	String -> Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
	Widget.Id -> ExprGuiM m (ExpressionGui m)
make name pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExpressionGui.stdWrap pl .
  	ExprGuiM.withFgColor (Config.atomColor config) . ExprGuiM.widgetEnv $
    ExpressionGui.fromValueWidget <$> BWidgets.makeFocusableTextView name myId
