module Lamdu.CodeEdit.ExpressionEdit.RecordEdit(make) where

import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE

make ::
  MonadA m =>
  Sugar.Record m (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make _record =
  fmap ExpressionGui.fromValueWidget .
    ExprGuiM.atEnv (WE.setTextColor Config.recordParensColor) .
    ExprGuiM.widgetEnv . BWidgets.makeFocusableTextView "{"
