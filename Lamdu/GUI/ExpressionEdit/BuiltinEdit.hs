module Lamdu.GUI.ExpressionEdit.BuiltinEdit(make) where

import           Control.MonadA (MonadA)
import qualified Data.List as List
import           Data.List.Split (splitOn)
import           Data.Monoid (Monoid(..))
import           Data.Store.Property (Property(..))
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import           Lamdu.GUI.ExpressionGui.Monad (WidgetT, ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [ModKey mempty GLFW.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Change imported name"]
  , FocusDelegator.stopDelegatingKeys = [ModKey mempty GLFW.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop changing name"]
  }

make
  :: MonadA m
  => Sugar.DefinitionBuiltin m
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
    dot <- ExprGuiM.makeLabel "." $ Widget.toAnimId myId
    return $ Box.hboxCentered [moduleName, dot, varName]
  where
    makeNamePartEditor color namePartStr setter makeWidgetId =
      ExprGuiM.withFgColor color .
      ExprGuiM.wrapDelegated builtinFDConfig FocusDelegator.NotDelegating id
      (ExprGuiM.widgetEnv .
       (BWidgets.makeWordEdit . Property namePartStr) setter) $
      makeWidgetId myId
    modulePathStr = List.intercalate "." modulePath
    modulePathSetter = setFFIName . (`Definition.FFIName` name) . splitOn "."
    nameSetter = setFFIName . Definition.FFIName modulePath
