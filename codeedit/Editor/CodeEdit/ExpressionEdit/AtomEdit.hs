module Editor.CodeEdit.ExpressionEdit.AtomEdit(make) where

import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.Config as Config
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => String
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
make name =
  liftM ExpressionGui.fromValueWidget .
    liftM (Widget.tint Config.atomColor) .
    BWidgets.makeFocusableTextView name
