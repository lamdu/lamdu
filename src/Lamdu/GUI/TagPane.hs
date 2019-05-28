module Lamdu.GUI.TagPane
    ( make
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

tagRenameId :: Widget.Id -> Widget.Id
tagRenameId = (`Widget.joinId` ["rename"])

disallowedNameChars :: String
disallowedNameChars = ",[]\\`()"

makeTagNameEdit ::
    ( MonadReader env m, Applicative f
    , Has (Texts.CodeUI Text) env
    , TextEdit.Deps env, GuiState.HasCursor env
    ) =>
    Name.StoredName f -> Widget.Id ->
    m (TextWidget f)
makeTagNameEdit (Name.StoredName prop tagText _tagCollision) myId =
    do
        env <- Lens.view id
        let stopEditingEventMap =
                E.keysEventMapMovesCursor
                [ MetaKey noMods MetaKey.Key'Escape
                , MetaKey noMods MetaKey.Key'Enter
                ]
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . Texts.stopEditing
                    ]
                ) (pure (TagEdit.tagViewId myId))
        TextEdits.makeWordEdit
            ?? TextEdit.Modes
                { TextEdit._unfocused = tagText ^. Name.ttText
                , TextEdit._focused = ""
                }
            ?? prop
            ?? tagRenameId myId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem`disallowedNameChars)
            <&> Align.tValue %~ Widget.weakerEvents stopEditingEventMap

make ::
    ( Monad i, Applicative o
    , Has (Texts.Name Text) env
    , Has (Texts.CodeUI Text) env
    , TextEdit.HasTexts env
    , Glue.HasTexts env
    ) =>
    Sugar.Tag (Name o) -> ExprGuiM env i o (TextWidget o)
make tag =
    do
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            TagEdit.makeTagView tag
        env <- Lens.view id
        let renameEventMap =
                tagRenameId myId
                & pure & E.keysEventMapMovesCursor
                (env ^. has . Config.jumpToDefinitionKeys)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . Texts.renameTag
                    ])
        let hover = Hover.hoverBeside Align.tValue ?? nameView
        case tag ^? Sugar.tagName . Name._Stored of
            Just storedName | isRenaming ->
                hover <*>
                (makeTagNameEdit storedName myId <&> (^. Align.tValue))
            _ -> nameView <&> Widget.weakerEvents renameEventMap & pure
        & GuiState.assignCursor myId viewId
    where
        myId = tag ^. Sugar.tagInstance & WidgetIds.fromEntityId
        viewId = TagEdit.tagViewId myId
