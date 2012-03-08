module Editor.CodeEdit.ParamEdit(make) where

import Data.Store.IRef (IRef)
import Editor.CTransaction (TWidget)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

make :: Monad m => IRef Data.Parameter -> TWidget t m
make paramI =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.parameterColor .
   BWidgets.makeNameEdit "<unnamed param>" paramI) $
  WidgetIds.fromIRef paramI
