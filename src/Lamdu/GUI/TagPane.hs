module Lamdu.GUI.TagPane
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import           Data.Property (Property, pVal)
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation.Id (AnimId, augmentId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.I18N.LangId (LangId(..), _LangId)
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
                ) (pure (TagView.id myId))
        TextEdits.makeWordEdit
            ?? empty
            ?? prop
            ?? tagRenameId myId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem` disallowedNameChars)
            <&> Align.tValue %~ Widget.weakerEvents stopEditingEventMap
    where
        empty = TextEdit.Modes
            { TextEdit._unfocused = tagText ^. Name.ttText
            , TextEdit._focused = ""
            }

makeTopRow ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.Name Text) env, Has (Texts.CodeUI Text) env
    , TextEdit.Deps env, Glue.HasTexts env
    , GuiState.HasCursor env, Has Theme env
    , Element.HasAnimIdPrefix env, Has Config.Config env, Has Hover.Style env
    ) =>
    Widget.Id -> Sugar.Tag (Name o) -> m (Gui Responsive o)
makeTopRow myId tag =
    do
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*> TagView.make tag
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        case tag ^? Sugar.tagName . Name._Stored of
            Just storedName | isRenaming ->
                (Hover.hoverBeside Align.tValue ?? nameView) <*>
                (makeTagNameEdit storedName myId <&> (^. Align.tValue))
            _ ->
                Lens.view id <&>
                \env ->
                let renameEventMap =
                        tagRenameId myId
                        & pure & E.keysEventMapMovesCursor
                        (env ^. has . Config.jumpToDefinitionKeys)
                        (E.toDoc env
                            [ has . MomentuTexts.edit
                            , has . Texts.tag
                            , has . Texts.renameTag
                            ])
                in nameView <&> Widget.weakerEvents renameEventMap
    & GuiState.assignCursor myId viewId
    <&> Responsive.fromWithTextPos
    where
        viewId = TagView.id myId

makeLanguageTitle ::
    ( MonadReader env m
    , Has TextView.Style env, Has Dir.Layout env
    , Has (Map LangId Text) env
    ) =>
    AnimId -> LangId -> m (Align.WithTextPos View)
makeLanguageTitle myId lang =
    TextView.make
    <*> (Lens.view has <&> getLang)
    <*> pure (myId <> ["lang-title"])
    where
        getLang :: Map LangId Text -> Text
        getLang x =
            x ^. Lens.at lang
            & fromMaybe (lang ^. _LangId & Lens.ix 0 %~ Char.toUpper)

makeLocalizedNames ::
    ( MonadReader env m
    , Applicative o
    , TextEdit.Deps env, Glue.HasTexts env, Spacer.HasStdSpacing env
    , Has LangId env, Has (Map LangId Text) env
    ) =>
    Widget.Id -> Map LangId (Property o Text) -> m (Gui Responsive o)
makeLocalizedNames myId names =
    do
        curLang <- Lens.view has
        let makeName lang name
                | lang == curLang = pure []
                | otherwise = makeLocalizedName lang name <&> (:[])
        Responsive.taggedList <*> (Lens.itraverse makeName names <&> (^.. traverse) <&> concat)
    where
        makeLocalizedName lang name =
            Responsive.TaggedItem
            <$> (makeLanguageTitle langId lang
                /|/ Spacer.stdHSpace
                <&> Align.tValue %~ Widget.fromView)
            <*> (TextView.make ?? name ^. pVal ?? langId <> ["val"] <&> Responsive.fromTextView)
            <*> pure Element.empty
            where
                langId = augmentId lang (Widget.toAnimId myId)

make ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.Name Text) env, Has (Texts.CodeUI Text) env
    , TextEdit.Deps env, Glue.HasTexts env
    , GuiState.HasCursor env, Has Theme env
    , Element.HasAnimIdPrefix env, Has Config.Config env
    , Has Hover.Style env, Spacer.HasStdSpacing env
    , Has LangId env, Has (Map LangId Text) env
    ) =>
    Sugar.TagPane (Name o) o -> m (Gui Responsive o)
make tagPane =
    Responsive.vbox <*>
    sequenceA
    [ makeTopRow myId (tagPane ^. Sugar.tpTag)
    , Spacer.stdVSpace <&> Responsive.fromView
    , makeLocalizedNames myId (tagPane ^. Sugar.tpLocalizedNames)
    ]
    where
        myId = tagPane ^. Sugar.tpTag . Sugar.tagInstance & WidgetIds.fromEntityId
