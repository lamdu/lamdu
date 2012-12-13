module Lamdu.CodeEdit.ExpressionEdit.BuiltinEdit(make) where

import Data.List.Split (splitOn)
import Data.Store.Property (Property(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (WidgetT, ExprGuiM)
import Control.MonadA (MonadA)
import qualified Data.List as List
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data as Data
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Change imported name"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop changing name"]
  }

make
  :: MonadA m
  => Sugar.DefinitionBuiltin m
  -> Widget.Id
  -> ExprGuiM m (WidgetT m)
make (Sugar.DefinitionBuiltin (Data.FFIName modulePath name) setFFIName) myId =
  ExprGuiM.assignCursor myId (WidgetIds.builtinFFIName myId) $ do
    moduleName <-
      makeNamePartEditor Config.foreignModuleColor
      modulePathStr modulePathSetter WidgetIds.builtinFFIPath
    varName <- makeNamePartEditor Config.foreignVarColor name nameSetter WidgetIds.builtinFFIName
    dot <- ExprGuiM.widgetEnv . BWidgets.makeLabel "." $ Widget.toAnimId myId
    return $ Box.hboxCentered [moduleName, dot, varName]
  where
    makeNamePartEditor color namePartStr mSetter makeWidgetId =
      ExprGuiM.atEnv (WE.setTextColor color) .
      ExprGuiM.wrapDelegated builtinFDConfig FocusDelegator.NotDelegating id
      (ExprGuiM.widgetEnv . maybe
       (BWidgets.makeTextView namePartStr . Widget.toAnimId)
       (BWidgets.makeWordEdit . Property namePartStr) mSetter) $
      makeWidgetId myId
    maybeSetter = (`fmap` setFFIName)
    modulePathStr = List.intercalate "." modulePath
    modulePathSetter = maybeSetter $ \ffiNameSetter ->
      ffiNameSetter . (`Data.FFIName` name) . splitOn "."
    nameSetter = maybeSetter $ \ffiNameSetter -> ffiNameSetter . Data.FFIName modulePath
