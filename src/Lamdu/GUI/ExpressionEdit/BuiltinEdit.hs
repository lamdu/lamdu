{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.BuiltinEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.View ((/|/))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextEdit.Property as TextEdits
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
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
    ExprGuiM m (WithTextPos (Widget (f Widget.EventResult)))
makeNamePartEditor color namePartStr setter myId =
    (FocusDelegator.make ?? builtinFDConfig ?? FocusDelegator.FocusEntryParent
     ?? myId <&> (Align.tValue %~))
    <*> ( TextEdits.makeWordEdit ?? empty ?? Property namePartStr setter ??
          myId `Widget.joinId` ["textedit"]
        )
    & Reader.local (TextView.color .~ color)
    where
        empty = TextEdit.EmptyStrings "unnamed builtin" ""

make ::
    Monad m =>
    Sugar.DefinitionBuiltin m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
make def myId =
    do
        theme <- Lens.view Theme.theme
        moduleName <-
            makeNamePartEditor (Theme.foreignModuleColor theme)
            modulePathStr modulePathSetter (builtinFFIPath myId)
        varName <-
            makeNamePartEditor (Theme.foreignVarColor theme) name nameSetter
            (builtinFFIName myId)
        dot <- TextView.makeLabel "."
        moduleName /|/ dot /|/ varName & return
    & Widget.assignCursor myId (builtinFFIName myId)
    where
        Sugar.DefinitionBuiltin
            (Definition.FFIName modulePath name) setFFIName _ = def
        modulePathStr = Text.intercalate "." modulePath
        modulePathSetter = setFFIName . (`Definition.FFIName` name) . Text.splitOn "."
        nameSetter = setFFIName . Definition.FFIName modulePath
