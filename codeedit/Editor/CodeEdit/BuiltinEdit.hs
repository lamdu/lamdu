module Editor.CodeEdit.BuiltinEdit(make) where

import Data.List.Split (splitOn)
import Data.Store.Property (Property(..))
import Editor.Anchors (ViewTag)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, WidgetT)
import qualified Data.List as List
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Change imported name"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Stop changing name"
  }

make
  :: MonadF m
  => Sugar.DefinitionBuiltin m
  -> Widget.Id
  -> OTransaction ViewTag m (WidgetT ViewTag m)
make (Sugar.DefinitionBuiltin (Data.FFIName modulePath name) setFFIName) myId =
  OT.assignCursor myId (WidgetIds.builtinFFIName myId) $ do
    moduleName <-
      makeNamePartEditor Config.foreignModuleColor
      modulePathStr modulePathSetter WidgetIds.builtinFFIPath
    varName <- makeNamePartEditor Config.foreignVarColor name nameSetter WidgetIds.builtinFFIName
    dot <- BWidgets.makeLabel "." $ Widget.toAnimId myId
    return $ BWidgets.hboxCentered [moduleName, dot, varName]
  where
    makeNamePartEditor color namePartStr mSetter makeWidgetId =
      OT.atEnv (BWidgets.setTextColor color) .
      BWidgets.wrapDelegated builtinFDConfig FocusDelegator.NotDelegating id
      (maybe
       (BWidgets.makeTextView namePartStr . Widget.toAnimId)
       (BWidgets.makeWordEdit . Property namePartStr) mSetter) $
      makeWidgetId myId
    maybeSetter f = fmap f setFFIName
    modulePathStr = List.intercalate "." modulePath
    modulePathSetter = maybeSetter $ \ffiNameSetter ->
      ffiNameSetter . (`Data.FFIName` name) . splitOn "."
    nameSetter = maybeSetter $ \ffiNameSetter -> ffiNameSetter . Data.FFIName modulePath
