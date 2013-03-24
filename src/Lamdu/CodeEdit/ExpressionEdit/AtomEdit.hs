module Lamdu.CodeEdit.ExpressionEdit.AtomEdit(make) where

import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Config as Config

make :: MonadA m => String -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make name =
  fmap ExpressionGui.fromValueWidget .
    ExprGuiM.withFgColor Config.atomColor . ExprGuiM.widgetEnv .
    BWidgets.makeFocusableTextView name
