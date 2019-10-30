module Lamdu.GUI.TagPane
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Binary.Extended (encodeS)
import qualified Data.Char as Char
import           Data.Property (Property(..))
import           GUI.Momentu.Align (TextWidget, Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import           Lamdu.Data.Tag (TextsInLang(..))
import qualified Lamdu.Data.Tag as Tag
import           Lamdu.GUI.Styled (addValFrame, label)
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
    Property f Text -> Widget.Id ->
    m (TextWidget f)
makeTagNameEdit prop myId =
    TextEdits.makeWordEdit
    ?? pure "  "
    ?? prop
    ?? tagRenameId myId
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem` disallowedNameChars)

makeFocusableTagNameEdit ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.CodeUI Text) env
    , TextEdit.Deps env
    , GuiState.HasCursor env
    , Has Config.Config env
    ) =>
    Widget.Id -> Property o Text -> m (TextWidget o)
makeFocusableTagNameEdit myId prop =
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
            <*> makeTagNameEdit prop myId

makeLanguageTitle ::
    ( MonadReader env m
    , Has TextView.Style env, Has Dir.Layout env
    , Has (Map LangId Text) env
    ) =>
    Widget.Id -> LangId -> m (Align.WithTextPos (Widget o))
makeLanguageTitle myId lang =
    TextView.make
    <*> (Lens.view has <&> getLang)
    <*> pure (Widget.toAnimId myId <> ["lang-title"])
    <&> Align.tValue %~ Widget.fromView
    where
        getLang :: Map LangId Text -> Text
        getLang x =
            x ^. Lens.at lang
            & fromMaybe (lang ^. _LangId & Lens.ix 0 %~ Char.toUpper)

data Row a = Row
    { _language :: a
    , _space0 :: a
    , _name :: a
    , _space1 :: a
    , _abbreviation :: a
    , _space2 :: a
    , _disambig :: a
    } deriving (Functor, Foldable, Traversable)

langWidgetId :: Widget.Id -> LangId -> Widget.Id
langWidgetId parentId lang =
    parentId `Widget.joinId` [encodeS lang]

nameId :: Widget.Id -> Widget.Id
nameId = (`Widget.joinId` ["name"])

hspace :: (MonadReader env m, Spacer.HasStdSpacing env) => m (WithTextPos (Widget f))
hspace =
    Spacer.stdHSpace <&> WithTextPos 0 <&> Align.tValue %~ Widget.fromView

row ::
    (MonadReader env m, Spacer.HasStdSpacing env, Functor f) =>
    m (WithTextPos (Widget f)) ->
    m (WithTextPos (Widget f)) ->
    m (WithTextPos (Widget f)) ->
    m (WithTextPos (Widget f)) ->
    m (Row (Aligned (Widget f)))
row lang name abbrev disambig =
    Row lang hspace name hspace abbrev hspace disambig
    & sequenceA
    <&> Lens.mapped %~ Align.fromWithTextPos 0

makeLangRow ::
    ( Applicative o
    , MonadReader env m
    , Spacer.HasStdSpacing env
    , Has (Map LangId Text) env, Has (Texts.CodeUI Text) env
    , TextEdit.Deps env, GuiState.HasCursor env, Has Config.Config env
    ) =>
    Widget.Id -> (LangId -> TextsInLang -> o ()) -> LangId -> TextsInLang ->
    m (Row (Aligned (Widget o)))
makeLangRow parentId setName lang langNames =
    row
    (makeLanguageTitle langId lang)
    (makeFocusableTagNameEdit (nameId langId) nameProp)
    (mkProp Tag.abbreviation & makeFocusableTagNameEdit (mkId "abbr"))
    (mkProp Tag.disambiguationText & makeFocusableTagNameEdit (mkId "disamb"))
    where
        mkId suffix = langId `Widget.joinId` [suffix]
        langId = langWidgetId parentId lang
        nameProp =
            setName lang . (\x -> langNames & Tag.name .~ x)
            & Property (langNames ^. Tag.name)
        mkProp l =
            setName lang .
            (\x -> langNames & Lens.cloneLens l .~ if x == "" then Nothing else Just x)
            & Property (langNames ^. Lens.cloneLens l . Lens._Just)

makeMissingLangRow ::
    ( Applicative o
    , MonadReader env m, Spacer.HasStdSpacing env
    , Has (Map LangId Text) env, Has (Texts.CodeUI Text) env
    , TextEdit.Deps env, GuiState.HasCursor env, Has Config.Config env
    ) =>
    Widget.Id -> (LangId -> TextsInLang -> o ()) -> LangId ->
    m (Row (Aligned (Widget o)))
makeMissingLangRow parentId setName lang =
    row
    (makeLanguageTitle langId lang)
    (makeFocusableTagNameEdit (nameId langId) nameProp)
    (pure Element.empty)
    (pure Element.empty)
    where
        langId = langWidgetId parentId lang
        nameProp =
            setName lang . (\x -> TextsInLang x Nothing Nothing)
            & Property ""

makeLangsTable ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.CodeUI Text) env, Has (Grid.Texts Text) env
    , TextEdit.Deps env, Glue.HasTexts env
    , GuiState.HasCursor env
    , Element.HasAnimIdPrefix env, Has Config.Config env
    , Spacer.HasStdSpacing env
    , Has LangId env, Has (Map LangId Text) env
    ) =>
    Widget.Id -> Map LangId TextsInLang ->
    (LangId -> TextsInLang -> o ()) -> m (Widget o)
makeLangsTable myId tagTexts setName =
    do
        lang <- Lens.view has
        let currentLang =
                case tagTexts ^. Lens.at lang of
                Nothing -> makeMissingLangRow myId setName lang
                Just cur -> makeLangRow myId setName lang cur
        let editOtherLangs =
                tagTexts ^@.. Lens.itraversed
                & filter ((/= lang) . fst)
                <&> uncurry (makeLangRow myId setName)
        Grid.make <*>
            sequence
            (heading : currentLang : editOtherLangs)
            <&> snd
    where
        -- the type of Styled.label is RankN, so duplicate a bit of
        -- code to avoid complicating too much here
        toWidget = fmap Widget.fromView
        heading =
            row
            (label MomentuTexts.language <&> toWidget)
            (label Texts.name <&> toWidget)
            (label Texts.abbreviation <&> toWidget)
            (label Texts.disambiguationText <&> toWidget)

make ::
    ( MonadReader env m
    , Applicative o
    , Has (Texts.CodeUI Text) env, Has (Grid.Texts Text) env
    , TextEdit.Deps env, Glue.HasTexts env
    , GuiState.HasCursor env, Has Theme env
    , Element.HasAnimIdPrefix env, Has Config.Config env
    , Spacer.HasStdSpacing env
    , Has LangId env, Has (Map LangId Text) env
    ) =>
    Sugar.TagPane Name o -> m (Widget o)
make tagPane =
    addValFrame <*>
    do
        lang <- Lens.view has
        makeLangsTable myId (tagPane ^. Sugar.tpTagData . Tag.tagTexts) (tagPane ^. Sugar.tpSetTexts)
            & GuiState.assignCursor myId (nameId (langWidgetId myId lang))
        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = tagPane ^. Sugar.tpTag . Sugar.tagInstance & WidgetIds.fromEntityId
