{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.BuiltinEdit
    ( make
    ) where

import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import           Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods GLFW.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Change imported name"]
    , FocusDelegator.focusParentKeys = [MetaKey noMods GLFW.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Stop changing name"]
    }

builtinFFIPath :: Widget.Id -> Widget.Id
builtinFFIPath = flip Widget.joinId ["FFIPath"]

builtinFFIName :: Widget.Id -> Widget.Id
builtinFFIName = flip Widget.joinId ["FFIName"]

makeNamePartEditor ::
    (Monad f, Monad m) =>
    Draw.Color -> Text -> (Text -> f ()) -> Widget.Id ->
    ExprGuiM m (Widget (f Widget.EventResult))
makeNamePartEditor color namePartStr setter myId =
    ExprGuiM.makeFocusDelegator builtinFDConfig FocusDelegator.FocusEntryParent myId
    <*> ExprGuiM.widgetEnv
        ( BWidgets.makeWordEdit empty (Property namePartStr setter)
          (myId `Widget.joinId` ["textedit"])
        )
    & ExprGuiM.localEnv (WE.textColor .~ color)
    where
        empty = TextEdit.EmptyStrings "unnamed builtin" ""

make ::
    Monad m =>
    Sugar.DefinitionBuiltin m -> Widget.Id ->
    ExprGuiM m (Widget (T m Widget.EventResult))
make def myId =
    do
        theme <- ExprGuiM.readTheme
        moduleName <-
            makeNamePartEditor (Theme.foreignModuleColor theme)
            modulePathStr modulePathSetter (builtinFFIPath myId)
        varName <-
            makeNamePartEditor (Theme.foreignVarColor theme) name nameSetter
            (builtinFFIName myId)
        dot <- ExprGuiM.makeLabel "." (Widget.toAnimId myId) <&> Widget.fromView
        [moduleName, dot, varName]
            & Box.hboxCentered
            & return
    & ExprGuiM.assignCursor myId (builtinFFIName myId)
    where
        Sugar.DefinitionBuiltin
            (Definition.FFIName modulePath name) setFFIName _ = def
        modulePathStr = Text.intercalate "." modulePath
        modulePathSetter = setFFIName . (`Definition.FFIName` name) . Text.splitOn "."
        nameSetter = setFFIName . Definition.FFIName modulePath
