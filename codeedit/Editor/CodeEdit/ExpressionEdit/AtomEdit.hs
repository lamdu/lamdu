module Editor.CodeEdit.ExpressionEdit.AtomEdit(make) where

import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Control.MonadA (MonadA)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.Config as Config
import qualified Editor.WidgetEnvT as WE
import qualified Graphics.UI.Bottle.Widget as Widget

make :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make name =
  fmap ExpressionGui.fromValueWidget .
    ExprGuiM.atEnv (WE.setTextColor Config.atomColor) . ExprGuiM.widgetEnv .
    BWidgets.makeFocusableTextView name
