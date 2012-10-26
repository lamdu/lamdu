module Editor.CodeEdit.ExpressionEdit.AtomEdit(make) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.Config as Config
import qualified Editor.WidgetEnvT as OT
import qualified Graphics.UI.Bottle.Widget as Widget

make :: MonadF m => String -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make name =
  liftM ExpressionGui.fromValueWidget .
    ExprGuiM.atEnv (OT.setTextColor Config.atomColor) . ExprGuiM.otransaction .
    BWidgets.makeFocusableTextView name
