module Lamdu.GUI.Expr.TagEdit
    ( makeColoredTag
    , makeParamTag, addItemId, makeLHSTag
    , makeArgTag
    , makeTagHoleEdit
    , makeBinderTagEdit
    , makeJumpToTagEventMap
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import qualified Data.Char as Char
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import           GUI.Momentu (EventMap, Update)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.Classes as C
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.I18N.UnicodeAlts (unicodeAlts)
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.EntityId (EntityId)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makePickEventMap :: _ => f Menu.PickResult -> m (EventMap (f M.Update))
makePickEventMap action =
    Lens.view id <&>
    \env ->
    let pickKeys = env ^. has . Menu.configKeysPickOption
        jumpNextKeys = env ^. has . Menu.configKeysPickOptionAndGotoNext
        mkDoc lens =
            E.toDoc env [has . MomentuTexts.edit, has . Texts.tag, has . lens]
    in
    E.keysEventMapMovesCursor pickKeys (mkDoc Texts.new)
    (action <&> (^. Menu.pickDest))
    -- TODO: DRY with search-menu?
    <> E.keyPresses jumpNextKeys
        (mkDoc Texts.newAndJumpToNextEntry)
        (action <&> \result ->
            case result ^. Menu.pickMNextEntry of
            Just nextEntry -> GuiState.updateCursor nextEntry
            Nothing ->
                GuiState.updateCursor (result ^. Menu.pickDest)
                & GuiState.uPreferStroll .~ True ^. Lens._Unwrapped
        )

makeNewTag ::
    _ =>
    Sugar.TagOption Name o ->
    m (Text -> (EntityId -> r) -> o r)
makeNewTag tagOpt =
    C.setTagName <&>
    \setName searchTerm mkPickResult ->
    mkPickResult (tagOpt ^. Sugar.toInfo . Sugar.tagInstance) <$
    do
        setName (tagOpt ^. Sugar.toInfo . Sugar.tagVal) searchTerm
        tagOpt ^. Sugar.toPick

makeNewTagPreEvent ::
    _ =>
    Sugar.TagOption Name o -> Text -> (EntityId -> r) ->
    m (Maybe (Widget.PreEvent (o r)))
makeNewTagPreEvent tagOpt searchTerm mkPickResult =
    (,) <$> Lens.view (has . Texts.newName) <*> makeNewTag tagOpt
    <&>
    \(newNameText, newTag) ->
    if Text.null searchTerm
    then Nothing
    else
        Just Widget.PreEvent
        { Widget._pDesc = newNameText
        , Widget._pAction = newTag (searchTerm & Lens.ix 0 %~ Char.toLower) mkPickResult
        , Widget._pTextRemainder = ""
        }

makeAddNewTag ::
    _ =>
    Sugar.TagOption Name o -> (EntityId -> Menu.PickResult) -> SearchMenu.ResultsContext ->
    m (Maybe (Menu.Option m o))
makeAddNewTag tagOpt mkPickResult ctx =
    makeNewTagPreEvent tagOpt searchTerm mkPickResult <&> Lens.mapped %~
    \preEvent ->
    Menu.Option
    { Menu._oId = optionId
    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
    , Menu._oRender =
        Styled.label Texts.createNew
        >>= M.tValue (Widget.makeFocusableView optionId)
        <&> (`Menu.RenderedOption` preEvent)
        & Styled.withColor TextColors.actionTextColor
    }
    where
        optionId = ctx ^. SearchMenu.rResultIdPrefix <> "Create new"
        searchTerm = ctx ^. SearchMenu.rSearchTerm

nameText :: Lens.Traversal' (Sugar.TagOption Name m) Text
nameText = Sugar.toInfo . Sugar.tagName . Name._NameTag . Name.tnDisplayText . Name.ttText

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

makeOptions ::
    _ =>
    Sugar.TagChoice Name o ->
    Sugar.TagOption Name o ->
    (EntityId -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    m (Menu.OptionList (Menu.Option m o))
makeOptions tagRefReplace newTagOpt mkPickResult ctx
    | Text.null searchTerm = pure Menu.OptionList { Menu._olIsTruncated = True, Menu._olOptions = [] }
    | otherwise =
        do
            resultCount <-
                Lens.view
                (Config.hasConfig . Config.completion . Config.completionResultCount)
            let nonFuzzyResults =
                    results ^? Lens.ix 0 . _1 . Fuzzy.isFuzzy
                    & any not
            addNewTag <- makeAddNewTag newTagOpt mkPickResult ctx
            let maybeAddNewTagOption
                    | nonFuzzyResults || not (Name.isValidSearchText searchTerm) = id
                    | otherwise =
                        maybe id (:) addNewTag
            chooseText <- Lens.view (has . MomentuTexts.choose)
            let makeOption opt =
                    Menu.Option
                    { Menu._oId = optionWId
                    , Menu._oRender =
                        NameView.make (opt ^. Sugar.toInfo . Sugar.tagName)
                        >>= M.tValue (Widget.makeFocusableView optionWId)
                        & local (M.elemIdPrefix .~ M.asElemId instanceId)
                        <&>
                        \widget ->
                        Menu.RenderedOption
                        { Menu._rWidget = widget
                        , Menu._rPick = Widget.PreEvent
                            { Widget._pDesc = chooseText
                            , Widget._pAction =
                                mkPickResult (opt ^. Sugar.toInfo . Sugar.tagInstance)
                                <$ opt ^. Sugar.toPick
                            , Widget._pTextRemainder = ""
                            }
                        }
                    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
                    }
                    where
                        instanceId =
                            opt ^. Sugar.toInfo . Sugar.tagInstance
                            & WidgetIds.fromEntityId
                        optionWId =
                            ctx ^. SearchMenu.rResultIdPrefix <> instanceId
            results <&> snd
                & splitAt resultCount
                & _2 %~ not . null
                & _1 %~ maybeAddNewTagOption . map makeOption
                & (\(list, isTrunc) -> Menu.OptionList isTrunc list)
                & pure
    where
        results = Fuzzy.memoableMake fuzzyMaker (tagRefReplace ^. Sugar.tcOptions <&> withTexts) searchTerm
        withTexts tagOption = (tagOption ^.. nameText >>= unicodeAlts, tagOption)
        searchTerm = ctx ^. SearchMenu.rSearchTerm

allowedSearchTerm :: Text -> Bool
allowedSearchTerm = Name.isValidSearchText

makeHoleSearchTerm ::
    _ =>
    Sugar.TagOption Name o ->
    (EntityId -> Menu.PickResult) -> ElemId ->
    m (SearchMenu.Term o)
makeHoleSearchTerm newTagOption mkPickResult holeId =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        let allowNewTag = Name.isValidText searchTerm
        newTag <- makeNewTag newTagOption
        newTagEventMap <-
            if allowNewTag
            then newTag searchTerm mkPickResult & makePickEventMap
            else pure mempty
        term <-
            SearchMenu.basicSearchTermEdit newTagId holeId (pure . allowedSearchTerm) SearchMenu.defaultEmptyStrings
            >>= SearchMenu.addDelSearchTerm holeId
            <&> SearchMenu.termWidget . M.tValue %~ Widget.weakerEvents newTagEventMap
        tooltip <- Lens.view (has . Theme.tooltip)
        if  allowNewTag &&
            Widget.isFocused (term ^. SearchMenu.termWidget . M.tValue)
            then
                do
                    newText <- Lens.view (has . Texts.new)
                    newTagLabel <- Element.subElemId "label" >>= TextView.make ("(" <> newText <> ")")
                    space <- Spacer.stdHSpace
                    Glue.Poly (|||) <- Glue.mkPoly Glue.Horizontal
                    hNewTagLabel <- Hover.hover newTagLabel <&> Hover.sequenceHover
                    anchor <- pushToReader Hover.anchor <&> fmap
                    let termWithHover termW =
                            let hoverOptions =
                                    [ anchor (termW ||| space) ||| hNewTagLabel
                                    , hNewTagLabel ||| anchor (space ||| termW)
                                    ] <&> (^. M.tValue)
                            in  anchor termW
                                <&> Hover.hoverInPlaceOf hoverOptions
                    term & SearchMenu.termWidget %~ termWithHover & pure
                    & local (Hover.backgroundColor .~ tooltip ^. Theme.tooltipBgColor)
                    & local (TextView.color .~ tooltip ^. Theme.tooltipFgColor)
                    & local (M.elemIdPrefix <>~ "label")
            else pure term
    where
        newTagId = newTagOption ^. Sugar.toInfo . Sugar.tagInstance & WidgetIds.fromEntityId & M.asElemId

makeTagHoleEdit ::
    _ =>
    (EntityId -> Menu.PickResult) ->
    ElemId ->
    Sugar.TagChoice Name o ->
    m (M.TextWidget o)
makeTagHoleEdit mkPickResult holeId tagRefReplace =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        newTagPreEvents <-
            makeNewTagPreEvent newTagOption searchTerm mkPickResult
            <&> (^.. Lens._Just) <&> Lens.mapped . Lens.mapped %~ (mempty <$)
        let setPreEvent [] = newTagPreEvents
            setPreEvent x = x
        SearchMenu.make
            (const (makeHoleSearchTerm newTagOption mkPickResult holeId))
            (makeOptions tagRefReplace newTagOption mkPickResult) M.empty holeId
            Menu.AnyPlace
            <&> M.tValue . Widget.wState . Widget._StateFocused . Lens.mapped . Widget.fPreEvents %~ setPreEvent
    where
        newTagOption = tagRefReplace ^. Sugar.tcNewTag

makeTagRefEdit :: _ => (Sugar.EntityId -> Maybe ElemId) -> Sugar.TagRef Name i o -> m (M.TextWidget o)
makeTagRefEdit onPickNext = makeTagRefEditWith id onPickNext Nothing <&> fmap snd

data TagRefEditType
    = TagHole
    | SimpleView
    deriving (Eq)

makeJumpToTagEventMap :: _ => o EntityId -> m (EventMap (o M.Update))
makeJumpToTagEventMap jump =
    Lens.view id <&>
    \env ->
    jump <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor
    (env ^. Config.hasConfig . Config.jumpToDefinitionKeys)
    (E.toDoc env
        [ has . MomentuTexts.edit
        , has . Texts.tag
        , has . Texts.jumpToTag
        ])

makeTagRefEditWith ::
    _ =>
    (m (M.TextWidget o) -> m (M.TextWidget o)) ->
    (Sugar.EntityId -> Maybe ElemId) ->
    Maybe (o EntityId) ->
    Sugar.TagRef Name i o ->
    m (TagRefEditType, M.TextWidget o)
makeTagRefEditWith onView onPickNext mSetToAnon tag =
    do
        isHole <- GuiState.isSubCursor holeId
        env <- Lens.view id
        let chooseNewTagEventMap =
                E.keysEventMapMovesCursor
                (Config.delKeys env <> env ^. Config.hasConfig . Config.jumpToDefinitionKeys)
                ( E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . MomentuTexts.choose
                    ] ) chooseAction
        jumpToTagEventMap <- foldMap makeJumpToTagEventMap (tag ^. Sugar.tagRefJumpTo)
        let eventMap = jumpToTagEventMap <> chooseNewTagEventMap
        nameView <-
            TagView.make info
            >>= M.tValue (Widget.makeFocusableView viewId)
            <&> Lens.mapped %~ Widget.weakerEvents eventMap
            & onView
        if isHole
            then
                tag ^. Sugar.tagRefReplace & C.liftInfo
                >>= makeTagHoleEdit mkPickResult holeId
                <&> (,) TagHole
            else pure (SimpleView, nameView)
        & GuiState.assignCursor myId viewId
    where
        info = tag ^. Sugar.tagRefTag
        myId = info ^. Sugar.tagInstance & WidgetIds.fromEntityId
        holeId = WidgetIds.tagHoleId myId
        viewId = myId <> "view"
        mkPickResult tagInstance =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId tagInstance
            , Menu._pickMNextEntry = onPickNext tagInstance
            }
        chooseAction =
            case mSetToAnon of
            Nothing -> pure myId
            Just setAnon -> setAnon <&> WidgetIds.fromEntityId
            <&> WidgetIds.tagHoleId

makeColoredTag ::
    _ => _ -> (EntityId -> Maybe ElemId) -> Sugar.TagRef Name i o -> m (M.TextWidget o)
makeColoredTag color onPickNext = makeTagRefEdit onPickNext <&> Styled.withColor color

addItemId :: ElemId -> ElemId
addItemId = (<> "add item")

makeChooseEventMap :: _ => ElemId -> m (EventMap (o Update))
makeChooseEventMap tagEditId =
    Lens.view id <&>
    \env ->
    E.charEventMap "Letter"
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.tag, has . MomentuTexts.choose])
    chooseWithChar
    where
        chooseWithChar c =
            SearchMenu.enterWithSearchTerm (Text.singleton c) tagEditId
            <$ guard (Char.isAlpha c)
            <&> pure

makeLHSTag ::
    _ =>
    (Sugar.EntityId -> Maybe ElemId) ->
    Lens.ALens' TextColors M.Color ->
    Maybe (o EntityId) -> Sugar.TagRef Name i o ->
    m (M.TextWidget o)
makeLHSTag onPickNext color mSetToAnon tag =
    do
        (tagEditType, tagEdit) <-
            makeTagRefEditWith
            -- Apply the name style only when the tag is a view. If it is
            -- a tag hole, the name style (indicating auto-name) makes no sense
            -- onView =
            (Styled.nameAtBinder (tag ^. Sugar.tagRefTag . Sugar.tagName) . Styled.withColor color)
            onPickNext mSetToAnon tag
            & Styled.withColor color
            & local (\env -> env & has .~ env ^. has . Style.nameAtBinder)
        chooseEventMap <- makeChooseEventMap (WidgetIds.tagHoleId myId)
        let eventMap =
                case tagEditType of
                SimpleView -> chooseEventMap
                _ -> mempty
        tagEdit <&> Widget.weakerEvents eventMap & pure
    where
        myId = tag ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId

makeParamTag :: _ => Maybe (o EntityId) -> Sugar.TagRef Name i o -> m (M.TextWidget o)
makeParamTag =
    makeLHSTag onPickNext TextColors.variableColor
    where
        onPickNext pos = WidgetIds.fromEntityId pos & addItemId & Just

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag :: _ => Sugar.Tag Name -> m (M.WithTextPos M.View)
makeArgTag tag =
    NameView.make (tag ^. Sugar.tagName)
    & Styled.withColor TextColors.argTagColor
    & local (M.elemIdPrefix .~ elemId)
    where
        elemId = WidgetIds.fromEntityId (tag ^. Sugar.tagInstance) & M.asElemId

makeBinderTagEdit ::
    _ =>
    Lens.ALens' TextColors M.Color -> Sugar.OptionalTag Name i o ->
    m (M.TextWidget o)
makeBinderTagEdit color (Sugar.OptionalTag tag setToAnon) =
    makeLHSTag (const Nothing) color (Just setToAnon) tag
    & local (has . Menu.configKeysPickOptionAndGotoNext .~ ([] :: [M.ModKey]))
