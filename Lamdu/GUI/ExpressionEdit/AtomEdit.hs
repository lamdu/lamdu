module Lamdu.GUI.ExpressionEdit.AtomEdit(make) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Lamdu.GUI.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE

make :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make name myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.withFgColor (Config.atomColor config) . ExprGuiM.widgetEnv $
    ExpressionGui.fromValueWidget <$> BWidgets.makeFocusableTextView name myId
