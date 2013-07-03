module Lamdu.GUI.ExpressionEdit.BuiltinEdit(make) where

import Control.MonadA (MonadA)
import Data.List.Split (splitOn)
import Data.Store.Property (Property(..))
import Lamdu.GUI.ExpressionGui.Monad (WidgetT, ExprGuiM)
import qualified Data.List as List
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Change imported name"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop changing name"]
  }

make
  :: MonadA m
  => Sugar.DefinitionBuiltin m (ExprGuiM.SugarExpr m)
  -> Widget.Id
  -> ExprGuiM m (WidgetT m)
make (Sugar.DefinitionBuiltin (Definition.FFIName modulePath name) setFFIName _) myId =
  ExprGuiM.assignCursor myId (WidgetIds.builtinFFIName myId) $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    moduleName <-
      makeNamePartEditor (Config.foreignModuleColor config)
      modulePathStr modulePathSetter WidgetIds.builtinFFIPath
    varName <-
      makeNamePartEditor (Config.foreignVarColor config) name nameSetter
      WidgetIds.builtinFFIName
    dot <- ExprGuiM.widgetEnv . BWidgets.makeLabel "." $ Widget.toAnimId myId
    return $ Box.hboxCentered [moduleName, dot, varName]
  where
    makeNamePartEditor color namePartStr mSetter makeWidgetId =
      ExprGuiM.withFgColor color .
      ExprGuiM.wrapDelegated builtinFDConfig FocusDelegator.NotDelegating id
      (ExprGuiM.widgetEnv . maybe
       (BWidgets.makeTextViewWidget namePartStr . Widget.toAnimId)
       (BWidgets.makeWordEdit . Property namePartStr) mSetter) $
      makeWidgetId myId
    maybeSetter = (`fmap` setFFIName)
    modulePathStr = List.intercalate "." modulePath
    modulePathSetter = maybeSetter $ \ffiNameSetter ->
      ffiNameSetter . (`Definition.FFIName` name) . splitOn "."
    nameSetter = maybeSetter $ \ffiNameSetter -> ffiNameSetter . Definition.FFIName modulePath
