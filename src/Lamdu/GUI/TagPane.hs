{-# LANGUAGE TypeFamilies #-}

module Lamdu.GUI.TagPane
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import           Data.Property (Property(..), pVal)
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Data.Text as Text
import           GUI.Momentu (TextWidget, Aligned(..), WithTextPos(..), Widget, (/-/), (/|/), noMods)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue (hbox)
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Data.Tag (TextsInLang(..))
import qualified Lamdu.Data.Tag as Tag
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.Styled (addValFrame, label, info, withColor)
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.I18N.LangId (LangId(..), _LangId)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

tagRenameId :: ElemId -> ElemId
tagRenameId = (<> "rename")

disallowedNameChars :: Set Char
disallowedNameChars = Set.fromList ",[]\\`()"

makeTagNameEdit :: _ => Property f Text -> ElemId -> m (TextWidget f)
makeTagNameEdit prop myId =
    TextEdits.makeWordEdit
    (pure "  ")
    (prop & Property.pSet %~ (. (Lens.ix 0 %~ Char.toLower)))
    (tagRenameId myId)
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`Set.notMember` disallowedNameChars)

makeSymbolNameEdit :: _ => Property f Text -> ElemId -> m (TextWidget f)
makeSymbolNameEdit prop myId =
    do
        empties <- Lens.view (has . Texts.typeOperatorHere) <&> pure
        TextEdits.makeWordEdit empties prop (tagRenameId myId)
    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars allowedSymbolChars
    where
        allowedSymbolChars =
            Set.member
            ?? Set.fromList Chars.operator `Set.difference` disallowedNameChars

makeFocusableTagNameEdit :: _ => ElemId -> Property o Text -> m (TextWidget o)
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
                    [ noMods ModKey.Key'Escape
                    , noMods ModKey.Key'Enter
                    ]
                , FocusDelegator.focusParentDoc =
                    E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . Texts.stopEditing
                    ]
                }
        makeTagNameEdit prop myId
            >>= Align.tValue (FocusDelegator.make fdConfig FocusDelegator.FocusEntryParent myId)

makeLanguageTitle :: _ => ElemId -> LangId -> m (TextWidget o)
makeLanguageTitle myId lang =
    Lens.view has <&> getLang
    >>= (`TextView.make` (myId <> "lang-title"))
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

langElemId :: ElemId -> LangId -> ElemId
langElemId parentId lang = parentId <> M.asElemId lang

nameId :: ElemId -> ElemId
nameId = (<> "name")

hspace :: _ => m (TextWidget f)
hspace = Spacer.stdHSpace <&> Widget.fromView <&> WithTextPos 0

hspaceOf :: Widget.R -> TextWidget f
hspaceOf w = Spacer.makeHorizontal w & Widget.fromView & WithTextPos 0

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
    ElemId -> (LangId -> TextsInLang -> o ()) -> LangId -> TextsInLang ->
    m (TextsRow (Aligned (Widget o)))
makeLangRow parentId setName lang langNames =
    textsRow
    (makeLanguageTitle langId lang & info)
    (makeFocusableTagNameEdit (nameId langId) nameProp)
    (mkProp Tag.abbreviation & makeFocusableTagNameEdit (mkId "abbr"))
    (mkProp Tag.disambiguationText & makeFocusableTagNameEdit (mkId "disamb"))
    where
        mkId suffix = langId <> suffix
        langId = langElemId parentId lang
        nameProp =
            setName lang . (\x -> langNames & Tag.name .~ x)
            & Property (langNames ^. Tag.name)
        mkProp l =
            setName lang .
            (\x -> langNames & Lens.cloneLens l .~ if x == "" then Nothing else Just x)
            & Property (langNames ^. Lens.cloneLens l . Lens._Just)

makeMissingLangRow ::
    _ =>
    ElemId -> (LangId -> TextsInLang -> o ()) -> LangId ->
    m (TextsRow (Aligned (Widget o)))
makeMissingLangRow parentId setName lang =
    textsRow
    (makeLanguageTitle langId lang & info)
    (makeFocusableTagNameEdit (nameId langId) nameProp)
    (pure Element.empty)
    (pure Element.empty)
    where
        langId = langElemId parentId lang
        nameProp =
            setName lang . (\x -> TextsInLang x Nothing Nothing)
            & Property ""

makeLangsTable ::
    (MonadReader env m, _) =>
    ElemId -> Map LangId TextsInLang ->
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
        sequence (heading : currentLang : editOtherLangs)
    >>= Grid.make <&> snd
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

makeSymbol ::
    _ => ElemId -> Property o Tag.Symbol -> m (TextWidget o, TextWidget o)
makeSymbol myId symProp =
    case symProp ^. pVal of
    Tag.NoSymbol ->
        flip (,) Element.empty <$> makeDropDownList NoSymbol (toSym "" "")
    Tag.UniversalSymbol text ->
        (,)
        <$> makeDropDownList UniversalSymbol (toSym text text)
        <*> nameEdit (Property text (set . Tag.UniversalSymbol)) "universal"
    Tag.DirectionalSymbol (Tag.DirOp ltr rtl) ->
        (,)
        <$> makeDropDownList DirectionalSymbol (toSym ltr rtl)
        <*>
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

        mkId suffix = myId <> suffix
        nameEdit prop = makeSymbolNameEdit prop . mkId
        focusableLabel l suffix = Lens.view (has . l) >>= (`TextView.makeFocusable` mkId suffix)
        makeDropDownList curType toTagSym =
            do
                noSymLabel <- focusableLabel Texts.noSymbol "nosym"
                uniLabel <- focusableLabel Texts.symbol "unisym"
                dirLabel <- focusableLabel Texts.directionalSymbol "dirsym"
                defConf <- Lens.view (has . Texts.symbolType) >>= DropDownList.defaultConfig
                DropDownList.make (Property curType (set . toTagSym))
                    [ (NoSymbol, noSymLabel)
                    , (UniversalSymbol, uniLabel)
                    , (DirectionalSymbol, dirLabel)
                    ] defConf (mkId "symType")
                & withColor TextColors.actionTextColor


parseInt :: Text -> Maybe Int
parseInt newText
    | newText /= Text.strip newText = Nothing
    | newText == "" = Just 0
    | otherwise = tryParse newText

makeIntEdit :: _ => ElemId -> Property o Int -> m (TextWidget o)
makeIntEdit myId prop =
    do
        text <-
            M.readWidgetState myId
            <&> (^? Lens._Just . Lens.filtered ((== Just prevVal) . parseInt))
            <&> fromMaybe prevValStr
        TextEdit.make (TextEdit.Modes "0" "0") text myId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
            -- Avoid taking keys that don't belong to us,
            -- so weakerEvents with them will work.
            E.filter (Lens.has Lens._Just . parseInt . fst)
            <&> Align.tValue . Widget.updates %~
            \(newText, eventRes) ->
                eventRes <> GuiState.updateWidgetState myId newText
                <$ (parseInt newText & parseAssert & Property.set prop)
    where
        parseAssert = error "parsing int failed" & fromMaybe
        prevVal = Property.value prop
        prevValStr = show prevVal & Text.pack

makeOrderEdit :: _ => ElemId -> Property o Int -> m (TextWidget o)
makeOrderEdit tagPaneId prop =
    info (label Texts.order) /|/ hspace /|/
    makeIntEdit orderEditId prop
    where
        orderEditId = tagPaneId <> "tagOrder"


make :: _ => Sugar.TagPane o -> ElemId -> m (Widget o)
make tagPane myId =
    Lens.view has >>=
    \lang ->
    addValFrame <*>
    do
        (symbol, nextLine) <- makeSymbol myId symbolProp
        langsTable <-
            makeLangsTable myId
            (tagPane ^. Sugar.tpTagData . Tag.tagTexts) (tagPane ^. Sugar.tpSetTexts)
        orderEdit <- makeOrderEdit myId orderProp
        let totalWidth =
                max (nextLine ^. Element.width) (langsTable ^. Element.width)
        let gap = totalWidth - (symbol ^. Element.width + orderEdit ^. Element.width)
        pure langsTable
            /-/ (hbox [symbol, hspaceOf gap, orderEdit] <&> (^. Align.tValue))
            /-/ pure (nextLine ^. Align.tValue)
    & local (Element.elemIdPrefix .~ M.asElemId myId)
    & GuiState.assignCursor myId (nameId (langElemId myId lang))
    where
        orderProp =
            Property
            (tagPane ^. Sugar.tpTagData . Tag.tagOrder)
            (tagPane ^. Sugar.tpSetOrder)
        symbolProp =
            Property
            (tagPane ^. Sugar.tpTagData . Tag.tagSymbol)
            (tagPane ^. Sugar.tpSetSymbol)
