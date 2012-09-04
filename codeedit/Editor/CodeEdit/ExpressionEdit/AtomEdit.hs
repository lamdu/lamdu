module Editor.CodeEdit.ExpressionEdit.AtomEdit(make) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Graphics.UI.Bottle.Widget as Widget

make :: MonadF m => String -> Widget.Id -> VarAccess m (ExpressionGui m)
make name =
  liftM ExpressionGui.fromValueWidget .
    VarAccess.atEnv (BWidgets.setTextColor Config.atomColor) . VarAccess.otransaction .
    BWidgets.makeFocusableTextView name
