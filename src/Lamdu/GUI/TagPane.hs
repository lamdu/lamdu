module Lamdu.GUI.TagPane
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Binary.Extended (encodeS)
import qualified Data.Char as Char
import           Data.Property (Property(..), pVal)
import qualified Data.Property as Property
import qualified Data.Set as Set
import           GUI.Momentu.Align (TextWidget, Aligned(..), WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Data.Tag (TextsInLang(..))
import qualified Lamdu.Data.Tag as Tag
import           Lamdu.GUI.Styled (addValFrame, label, info, withColor)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.I18N.LangId (LangId(..), _LangId)
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

tagRenameId :: Widget.Id -> Widget.Id
tagRenameId = (`Widget.joinId` ["rename"])

disallowedNameChars :: Set Char
disallowedNameChars = Set.fromList ",[]\\`()"

makeTagNameEdit :: _ => Property f Text -> Widget.Id -> m (TextWidget f)
makeTagNameEdit prop myId =
    TextEdits.makeWordEdit
    ?? pure "  "
    ?? prop
    ?? tagRenameId myId
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`Set.notMember` disallowedNameChars)

makeSymbolNameEdit :: _ => Property f Text -> Widget.Id -> m (TextWidget f)
makeSymbolNameEdit prop myId =
    TextEdits.makeWordEdit
    <*> (Lens.view (has . Texts.typeOperatorHere) <&> pure)
    ?? prop
    ?? tagRenameId myId
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars allowedSymbolChars
    where
        allowedSymbolChars =
            Set.member
            ?? Set.fromList Chars.operator `Set.difference` disallowedNameChars

makeFocusableTagNameEdit :: _ => Widget.Id -> Property o Text -> m (TextWidget o)
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

makeLanguageTitle :: _ => Widget.Id -> LangId -> m (TextWidget o)
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

data TextsRow a = TextsRow
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

hspace :: _ => m (TextWidget f)
hspace =
    Spacer.stdHSpace <&> WithTextPos 0 <&> Align.tValue %~ Widget.fromView

textsRow ::
    _ =>
    m (TextWidget f) ->
    m (TextWidget f) ->
    m (TextWidget f) ->
    m (TextWidget f) ->
    m (TextsRow (Aligned (Widget f)))
textsRow lang name abbrev disambig =
    TextsRow lang hspace name hspace abbrev hspace disambig
    & sequenceA
    <&> Lens.mapped %~ Align.fromWithTextPos 0

makeLangRow ::
    _ =>
    Widget.Id -> (LangId -> TextsInLang -> o ()) -> LangId -> TextsInLang ->
    m (TextsRow (Aligned (Widget o)))
makeLangRow parentId setName lang langNames =
    textsRow
    (makeLanguageTitle langId lang & info)
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
    _ =>
    Widget.Id -> (LangId -> TextsInLang -> o ()) -> LangId ->
    m (TextsRow (Aligned (Widget o)))
makeMissingLangRow parentId setName lang =
    textsRow
    (makeLanguageTitle langId lang & info)
    (makeFocusableTagNameEdit (nameId langId) nameProp)
    (pure Element.empty)
    (pure Element.empty)
    where
        langId = langWidgetId parentId lang
        nameProp =
            setName lang . (\x -> TextsInLang x Nothing Nothing)
            & Property ""

makeLangsTable ::
    (MonadReader env m, _) =>
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
            textsRow
            (label MomentuTexts.language <&> toWidget)
            (label Texts.name <&> toWidget)
            (label Texts.abbreviation <&> toWidget)
            (label Texts.disambiguationText <&> toWidget)
            & info

data SymType = NoSymbol | UniversalSymbol | DirectionalSymbol
    deriving Eq

makeSymbol :: _ => Widget.Id -> Property o Tag.Symbol -> m (TextWidget o)
makeSymbol myId symProp =
    case symProp ^. pVal of
    Tag.NoSymbol -> makeChoice NoSymbol (toSym "" "")
    Tag.UniversalSymbol text ->
        makeChoice UniversalSymbol (toSym text text)
        /-/ nameEdit (Property text (set . Tag.UniversalSymbol)) "universal"
    Tag.DirectionalSymbol (Tag.DirOp ltr rtl) ->
        makeChoice DirectionalSymbol (toSym ltr rtl)
        /-/
        ( (label Texts.leftToRightSymbol & info <&> fmap Widget.fromView)
            /|/ hspace /|/ nameEdit (Property ltr (`setDirectional` rtl)) "ltr"
            /|/ hspace /|/ info (label Texts.rightToLeftSymbol)
            /|/ hspace /|/ nameEdit (Property rtl (setDirectional ltr)) "rtl"
        )
    where
        set = void . Property.set symProp
        setDirectional ltr rtl = Tag.DirOp ltr rtl & Tag.DirectionalSymbol & set

        toSym _ _ NoSymbol = Tag.NoSymbol
        toSym "" rtl UniversalSymbol = Tag.UniversalSymbol rtl
        toSym ltr _ UniversalSymbol = Tag.UniversalSymbol ltr
        toSym ltr rtl DirectionalSymbol = Tag.DirectionalSymbol (Tag.DirOp ltr rtl)

        mkId suffix = myId `Widget.joinId` [suffix]
        nameEdit prop = makeSymbolNameEdit prop . mkId
        focusableLabel l suffix =
            TextView.makeFocusable <*> Lens.view (has . l) ?? mkId suffix
        makeChoice curType toTagSym =
            do
                noSymLabel <- focusableLabel Texts.noSymbol "nosym"
                uniLabel <- focusableLabel Texts.symbol "unisym"
                dirLabel <- focusableLabel Texts.directionalSymbol "dirsym"
                defConf <- Choice.defaultConfig <*> Lens.view (has . Texts.symbolType)
                Choice.make ?? Property curType (set . toTagSym)
                    ?? [ (NoSymbol, noSymLabel)
                       , (UniversalSymbol, uniLabel)
                       , (DirectionalSymbol, dirLabel)
                       ]
                    ?? defConf ?? mkId "symType"
                & withColor TextColors.actionTextColor

make :: _ => Sugar.TagPane Name o -> m (Widget o)
make tagPane =
    addValFrame <*>
    do
        lang <- Lens.view has
        makeLangsTable myId
            (tagPane ^. Sugar.tpTagData . Tag.tagTexts) (tagPane ^. Sugar.tpSetTexts)
            /-/ (makeSymbol myId symbolProp <&> (^. Align.tValue))
            & GuiState.assignCursor myId (nameId (langWidgetId myId lang))
        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        symbolProp =
            Property
            (tagPane ^. Sugar.tpTagData . Tag.tagSymbol)
            (tagPane ^. Sugar.tpSetSymbol)
        myId = tagPane ^. Sugar.tpTag . Sugar.tagInstance & WidgetIds.fromEntityId
