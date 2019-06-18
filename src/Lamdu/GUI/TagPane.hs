module Lamdu.GUI.TagPane
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Char as Char
import           Data.Property (Property(..))
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation.Id (AnimId, augmentId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedList)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.I18N.LangId (LangId(..), _LangId)
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

tagRenameId :: Widget.Id -> Widget.Id
tagRenameId = (`Widget.joinId` ["rename"])

disallowedNameChars :: String
disallowedNameChars = ",[]\\`()"

makeTagNameEdit ::
    ( MonadReader env m, Applicative f
    , TextEdit.Deps env, GuiState.HasCursor env
    ) =>
    Property f Text -> Text -> Widget.Id ->
    m (TextWidget f)
makeTagNameEdit prop emptyText myId =
    TextEdits.makeWordEdit
    ?? empty
    ?? prop
    ?? tagRenameId myId
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem` disallowedNameChars)
    where
        empty = TextEdit.Modes
            { TextEdit._unfocused = emptyText
            , TextEdit._focused = ""
            }

makeTopRow ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.CodeUI Text) env
    , TextEdit.Deps env
    , GuiState.HasCursor env
    , Has Config.Config env
    ) =>
    Property o Text -> Text -> Widget.Id -> m (Responsive o)
makeTopRow prop emptyText myId =
    do
        env <- Lens.view id
        let fdConfig =
                FocusDelegator.Config
                { FocusDelegator.focusChildKeys = env ^. has . Config.jumpToDefinitionKeys
                , FocusDelegator.focusChildDoc =
                    E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . Texts.renameTag
                    ]
                , FocusDelegator.focusParentKeys =
                    [ MetaKey noMods MetaKey.Key'Escape
                    , MetaKey noMods MetaKey.Key'Enter
                    ]
                , FocusDelegator.focusParentDoc =
                    E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . Texts.stopEditing
                    ]
                }
        (FocusDelegator.make ?? fdConfig ?? FocusDelegator.FocusEntryParent ?? myId
            <&> (Align.tValue %~))
            <*> makeTagNameEdit prop emptyText (tagRenameId myId)
    <&> Responsive.fromWithTextPos


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
    Widget.Id -> Map LangId Text -> m (Responsive o)
makeLocalizedNames myId names =
    do
        curLang <- Lens.view has
        let makeName lang name
                | lang == curLang = pure []
                | otherwise = makeLocalizedName lang name <&> (:[])
        taggedList <*> (Lens.itraverse makeName names <&> (^.. traverse) <&> concat)
    where
        makeLocalizedName lang name =
            TaggedItem
            <$> (makeLanguageTitle langId lang
                /|/ Spacer.stdHSpace
                <&> Align.tValue %~ Widget.fromView
                <&> Just)
            <*> (TextView.make ?? name ?? langId <> ["val"] <&> Responsive.fromTextView)
            <*> pure Nothing
            where
                langId = augmentId lang (Widget.toAnimId myId)

make ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.CodeUI Text) env
    , TextEdit.Deps env, Glue.HasTexts env
    , GuiState.HasCursor env, Has Theme env
    , Element.HasAnimIdPrefix env, Has Config.Config env
    , Spacer.HasStdSpacing env
    , Has LangId env, Has (Map LangId Text) env
    ) =>
    Sugar.TagPane Name o -> m (Responsive o)
make tagPane =
    Styled.addValFrame <*>
    do
        lang <- Lens.view has
        let prop =
                Property
                (tagPane ^. Sugar.tpLocalizedNames . Lens.ix lang)
                ((tagPane ^. Sugar.tpSetName) lang)
        Responsive.vbox <*>
            sequenceA
            [ makeTopRow prop fallback myId
            , Spacer.stdVSpace <&> Responsive.fromView
            , makeLocalizedNames myId (tagPane ^. Sugar.tpLocalizedNames)
            ]
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = tagPane ^. Sugar.tpTag . Sugar.tagInstance & WidgetIds.fromEntityId
        fallback = tagPane ^. Sugar.tpLocalizedNames . Lens.ix (LangId "english")
