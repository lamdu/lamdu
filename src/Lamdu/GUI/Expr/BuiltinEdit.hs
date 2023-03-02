module Lamdu.GUI.Expr.BuiltinEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Text as Text
import           GUI.Momentu (noMods)
import qualified GUI.Momentu as M
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

builtinFDConfig :: _ => env -> FocusDelegator.Config
builtinFDConfig env = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [noMods ModKey.Key'Enter]
    , FocusDelegator.focusChildDoc = doc Texts.changeImportedName
    , FocusDelegator.focusParentKeys = [noMods ModKey.Key'Escape]
    , FocusDelegator.focusParentDoc = doc Texts.doneChangingImportedName
    }
    where
        doc lens = E.toDoc env [has . MomentuTexts.edit, has . lens]

builtinFFIPath :: ElemId -> ElemId
builtinFFIPath = (<> "FFIPath")

builtinFFIName :: ElemId -> ElemId
builtinFFIName = (<> "FFIName")

makeNamePartEditor ::
    _ => M.Color -> Text -> (Text -> f ()) -> ElemId -> m (M.TextWidget f)
makeNamePartEditor color namePartStr setter myId =
    do
        fdConfig <- Lens.view id <&> builtinFDConfig
        TextEdits.makeWordEdit empty (Property namePartStr setter) (myId <> "textedit")
            >>= M.tValue (FocusDelegator.make fdConfig FocusDelegator.FocusEntryParent myId)
    & local (TextView.color .~ color)
    where
        empty =
            TextEdit.Modes
            { TextEdit._unfocused = "(?)"
            , TextEdit._focused = ""
            }

make :: _ => Sugar.DefinitionBuiltin name o -> ElemId -> f (M.TextWidget o)
make def myId =
    do
        colors <- Lens.view (has . Theme.textColors)
        makeNamePartEditor (colors ^. TextColors.foreignModuleColor)
            modulePathStr modulePathSetter (builtinFFIPath myId)
            M./|/ Label.make "."
            M./|/ makeNamePartEditor (colors ^. TextColors.foreignVarColor) name
            nameSetter (builtinFFIName myId)
    & GuiState.assignCursor myId (builtinFFIName myId)
    where
        Sugar.DefinitionBuiltin
            (Definition.FFIName modulePath name) setFFIName _ = def
        modulePathStr = Text.intercalate "." modulePath
        modulePathSetter = setFFIName . (`Definition.FFIName` name) . Text.splitOn "."
        nameSetter = setFFIName . Definition.FFIName modulePath
