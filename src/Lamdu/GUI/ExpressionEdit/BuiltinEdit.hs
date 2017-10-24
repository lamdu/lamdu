{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.BuiltinEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

builtinFDConfig :: FocusDelegator.Config
builtinFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Change imported name"]
    , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Stop changing name"]
    }

builtinFFIPath :: Widget.Id -> Widget.Id
builtinFFIPath = flip Widget.joinId ["FFIPath"]

builtinFFIName :: Widget.Id -> Widget.Id
builtinFFIName = flip Widget.joinId ["FFIName"]

makeNamePartEditor ::
    ( Applicative f, MonadReader env m, Widget.HasCursor env
    , TextEdit.HasStyle env
    ) =>
    MDraw.Color -> Text -> (Text -> f ()) -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
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
    ( MonadReader env f, HasTheme env, Widget.HasCursor env
    , TextEdit.HasStyle env, Element.HasAnimIdPrefix env, Monad m
    ) =>
    Sugar.DefinitionBuiltin (T m) -> Widget.Id ->
    f (WithTextPos (Widget (T m GuiState.Update)))
make def myId =
    do
        colors <- Lens.view Theme.theme <&> Theme.codeForegroundColors
        moduleName <-
            makeNamePartEditor (Theme.foreignModuleColor colors)
            modulePathStr modulePathSetter (builtinFFIPath myId)
        varName <-
            makeNamePartEditor (Theme.foreignVarColor colors) name nameSetter
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
