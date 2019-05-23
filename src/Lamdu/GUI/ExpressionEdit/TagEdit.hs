{-# LANGUAGE ConstraintKinds #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeVariantTag, makeTagView
    , makeParamTag, addParamId
    , makeArgTag
    , makeTagHoleEdit
    , makeBinderTagEdit
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Char as Char
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing)
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.EntityId (EntityId)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

tagRenameId :: Widget.Id -> Widget.Id
tagRenameId = (`Widget.joinId` ["rename"])

tagViewId :: Widget.Id -> Widget.Id
tagViewId = (`Widget.joinId` ["view"])

disallowedNameChars :: String
disallowedNameChars = ",[]\\`()"

makeTagNameEdit ::
    ( MonadReader env m, Applicative f
    , Has (Texts.CodeUI Text) env
    , TextEdit.HasStyle env, GuiState.HasCursor env, TextEdit.HasTexts env
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
                ) (pure (tagViewId myId))
        TextEdits.makeWordEdit
            ?? TextEdit.Modes
                { TextEdit._unfocused = tagText ^. Name.ttText
                , TextEdit._focused = ""
                }
            ?? prop
            ?? tagRenameId myId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem`disallowedNameChars)
            <&> Align.tValue %~ Widget.weakerEvents stopEditingEventMap

tagId :: Sugar.TagRef name i o -> Widget.Id
tagId tag = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId

makePickEventMap ::
    ( Functor f, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , MonadReader env m
    ) =>
    f Menu.PickResult ->
    m (Gui EventMap f)
makePickEventMap action =
    Lens.view id <&>
    \env ->
    let config = env ^. has . Config.menu
        pickKeys = config ^. Menu.keysPickOption
        jumpNextKeys = config ^. Menu.keysPickOptionAndGotoNext
        mkDoc lens =
            E.toDoc env [has . MomentuTexts.edit, has . Texts.tag, has . lens]
    in
    E.keysEventMapMovesCursor pickKeys (mkDoc Texts.new)
    (action <&> (^. Menu.pickDest))
    -- TODO: DRY with search-menu?
    <> E.keyPresses (jumpNextKeys <&> MetaKey.toModKey)
        (mkDoc Texts.newAndJumpToNextEntry)
        (action <&> \result ->
            case result ^. Menu.pickMNextEntry of
            Just nextEntry -> GuiState.updateCursor nextEntry
            Nothing ->
                GuiState.updateCursor (result ^. Menu.pickDest)
                & GuiState.uPreferStroll .~ (True ^. Lens._Unwrapped)
        )

makeNewTag ::
    Functor o =>
    Text -> Sugar.TagReplace (Name o) i o a ->
    (EntityId -> a -> b) -> o b
makeNewTag searchTerm tagReplace mkPickResult =
    (tagReplace ^. Sugar.tsNewTag) searchTerm <&> uncurry mkPickResult

makeNewTagPreEvent ::
    ( Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    env -> Text -> Sugar.TagReplace (Name o) i o a ->
    (EntityId -> a -> r) -> Maybe (Widget.PreEvent (o r))
makeNewTagPreEvent env searchTerm tagReplace mkPickResult
    | Text.null searchTerm = Nothing
    | otherwise =
        Just Widget.PreEvent
        { Widget._pDesc = env ^. has . Texts.newName
        , Widget._pAction = makeNewTag searchTerm tagReplace mkPickResult
        , Widget._pTextRemainder = ""
        }

addNewTag ::
    ( Applicative o, MonadReader env f, GuiState.HasCursor env, Has Theme env
    , Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.CodeUI Text) menv
    ) =>
    menv -> Sugar.TagReplace (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    Maybe (Menu.Option f o)
addNewTag env tagReplace mkPickResult ctx =
    makeNewTagPreEvent env searchTerm tagReplace mkPickResult
    <&> \preEvent ->
    Menu.Option
    { Menu._oId = optionId
    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
    , Menu._oRender =
        (Widget.makeFocusableView ?? optionId <&> fmap)
        <*> Styled.label Texts.createNew
        <&> (`Menu.RenderedOption` preEvent)
        & Styled.withColor TextColors.actionTextColor
    }
    where
        optionId =
            (ctx ^. SearchMenu.rResultIdPrefix) `Widget.joinId` ["Create new"]
        searchTerm = ctx ^. SearchMenu.rSearchTerm

nameText :: Lens.Traversal' (Sugar.TagOption (Name f) m a) Text
nameText = Sugar.toInfo . Sugar.tagName . Name._Stored . Name.snDisplayText . Name.ttText

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

makeOptions ::
    ( Monad i, Applicative o, MonadReader menv m
    , GuiState.HasCursor menv, Has Theme menv, Has TextView.Style menv
    , Element.HasAnimIdPrefix menv, Glue.HasTexts menv
    , Has (Texts.Name Text) menv
    , Has (Texts.CodeUI Text) menv
    , Has (Texts.CodeUI Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    Sugar.TagReplace (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    ExprGuiM env i o (Menu.OptionList (Menu.Option m o))
makeOptions tagReplace mkPickResult ctx
    | Text.null searchTerm = pure Menu.TooMany
    | otherwise =
        do
            resultCount <-
                Lens.view
                (has . Config.completion . Config.completionResultCount)
            results <-
                tagReplace ^. Sugar.tsOptions
                <&> concatMap withText
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                & ExprGuiM.im
            let nonFuzzyResults =
                    results ^? Lens.ix 0 . Lens._1 . Fuzzy.isFuzzy
                    & maybe False not
            env <- Lens.view id
            let maybeAddNewTagOption
                    | nonFuzzyResults || not (allowedTagName searchTerm) = id
                    | otherwise =
                        maybe id (:) (addNewTag env tagReplace mkPickResult ctx)
            let makeOption opt =
                    Menu.Option
                    { Menu._oId = optionWId
                    , Menu._oRender =
                        (Widget.makeFocusableView ?? optionWId <&> fmap)
                        <*> NameView.make (opt ^. Sugar.toInfo . Sugar.tagName)
                        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId instanceId)
                        <&>
                        \widget ->
                        Menu.RenderedOption
                        { Menu._rWidget = widget
                        , Menu._rPick = Widget.PreEvent
                            { Widget._pDesc =
                                env ^. has . MomentuTexts.choose
                            , Widget._pAction =
                                opt ^. Sugar.toPick
                                <&> mkPickResult
                                (opt ^. Sugar.toInfo . Sugar.tagInstance)
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
                & uncurry Menu.toOptionList
                & pure
    where
        withText tagOption = tagOption ^.. nameText <&> ((,) ?? tagOption)
        searchTerm = ctx ^. SearchMenu.rSearchTerm

allowedSearchTerm :: Text -> Bool
allowedSearchTerm = Name.isValidText

-- Allowed name for tag assuming it is already a valid search term
allowedTagName :: Text -> Bool
allowedTagName =
    Lens.anyOf (Lens.ix 0) f
    where
        f x = Char.isAlpha x || elem x Chars.operator

type HasSearchTermEnv env =
    ( Has Theme env, Has Config env, GuiState.HasState env
    , TextEdit.HasStyle env, Has Hover.Style env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    )

makeHoleSearchTerm ::
    ( MonadReader env m, Applicative o, HasSearchTermEnv env, Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    ) =>
    Sugar.TagReplace (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) -> Widget.Id ->
    m (SearchMenu.Term o)
makeHoleSearchTerm tagReplace mkPickResult holeId =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        let allowNewTag = allowedTagName searchTerm
        newTagEventMap <-
            if allowNewTag
            then makeNewTag searchTerm tagReplace mkPickResult & makePickEventMap
            else pure mempty
        env <- Lens.view id
        let newTagPreEvents =
                makeNewTagPreEvent env searchTerm tagReplace mkPickResult
                ^.. Lens._Just
                <&> fmap (mempty <$)
        let addPreEvents =
                Widget.wState . Widget._StateFocused . Lens.mapped .
                Widget.fPreEvents %~ (Widget.PreEvents newTagPreEvents <>)
        term <-
            SearchMenu.addDelSearchTerm holeId
            <*> SearchMenu.basicSearchTermEdit holeId (pure . allowedSearchTerm)
                SearchMenu.defaultEmptyStrings
            <&> SearchMenu.termWidget . Align.tValue %~
                addPreEvents . Widget.weakerEvents newTagEventMap
        tooltip <- Lens.view (has . Theme.tooltip)
        if  allowNewTag &&
            Widget.isFocused (term ^. SearchMenu.termWidget . Align.tValue)
            then
                do
                    newTagLabel <-
                        (TextView.make ?? ("(" <> env ^. has . Texts.new <> ")"))
                            <*> (Element.subAnimId ?? ["label"])
                    space <- Spacer.stdHSpace
                    hover <- Hover.hover
                    Glue.Poly (|||) <- Glue.mkPoly ?? Glue.Horizontal
                    anchor <- Hover.anchor <&> fmap
                    let hNewTagLabel = hover newTagLabel & Hover.sequenceHover
                    let termWithHover termW =
                            let hoverOptions =
                                    [ anchor (termW ||| space) ||| hNewTagLabel
                                    , hNewTagLabel ||| anchor (space ||| termW)
                                    ] <&> (^. Align.tValue)
                            in  anchor termW
                                <&> Hover.hoverInPlaceOf hoverOptions
                    term & SearchMenu.termWidget %~ termWithHover & pure
                    & Reader.local (Hover.backgroundColor .~ tooltip ^. Theme.tooltipBgColor)
                    & Reader.local (TextView.color .~ tooltip ^. Theme.tooltipFgColor)
                    & Reader.local (Element.animIdPrefix <>~ ["label"])
            else pure term

makeTagHoleEdit ::
    ( Monad i, Applicative o
    , Has (Texts.Name Text) env
    , Has (Texts.CodeUI Text) env
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    Sugar.TagReplace (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    Widget.Id ->
    ExprGuiM env i o (TextWidget o)
makeTagHoleEdit tagReplace mkPickResult holeId =
    SearchMenu.make
    (const (makeHoleSearchTerm tagReplace mkPickResult holeId))
    (makeOptions tagReplace mkPickResult) Element.empty holeId
    ?? Menu.AnyPlace

makeTagView ::
    ( MonadReader env m, Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has Theme env, Has Dir.Layout env, Has (Texts.Name Text) env
    ) =>
    Sugar.TagInfo (Name f) -> m (WithTextPos View)
makeTagView tag =
    NameView.make (tag ^. Sugar.tagName)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId =
            tag ^. Sugar.tagInstance
            & WidgetIds.fromEntityId
            & Widget.toAnimId

makeTagEdit ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TextWidget o)
makeTagEdit = makeTagEditWith id (const Nothing) <&> fmap snd

data TagEditType
    = TagRename
    | TagHole
    | SimpleView
    deriving (Eq)

makeTagEditWith ::
    ( Monad i, Applicative o, MonadReader nenv n
    , GuiState.HasCursor nenv, Has TextView.Style nenv, Has (Texts.Name Text) nenv
    , Element.HasAnimIdPrefix nenv, Has Theme nenv, Glue.HasTexts nenv
    , Glue.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Navigation Text) env
    , Has (Texts.Name Text) env
    ) =>
    (n (TextWidget o) ->
     ExprGuiM env i o (TextWidget o)) ->
    (Sugar.EntityId -> Maybe Widget.Id) ->
    Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TagEditType, TextWidget o)
makeTagEditWith onView onPickNext tag =
    do
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        let mRenamingStoredName
                | isRenaming = tag ^? Sugar.tagInfo . Sugar.tagName . Name._Stored
                | otherwise = Nothing
        isHole <- GuiState.isSubCursor ?? WidgetIds.tagHoleId myId
        env <- Lens.view id
        let eventMap =
                ( case tag ^. Sugar.tagInfo . Sugar.tagName of
                    Name.Stored{} ->
                        E.keysEventMapMovesCursor
                        (env ^. has . Config.jumpToDefinitionKeys)
                        (E.toDoc env
                            [ has . MomentuTexts.edit
                            , has . Texts.tag
                            , has . Texts.renameTag
                            ])
                        (pure (tagRenameId myId))
                    _ -> mempty
                )
                <>
                E.keysEventMapMovesCursor
                (Config.delKeys env <> env ^. has . Config.jumpToDefinitionKeys)
                ( E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . MomentuTexts.choose
                    ] ) chooseAction
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            makeTagView (tag ^. Sugar.tagInfo)
            <&> Lens.mapped %~ Widget.weakerEvents eventMap
            & onView
        let hover = Hover.hoverBeside Align.tValue ?? nameView
        case mRenamingStoredName of
            Just storedName ->
                hover <*>
                (makeTagNameEdit storedName myId <&> (^. Align.tValue))
                <&> (,) TagRename
            Nothing
                | isHole ->
                    makeTagHoleEdit (tag ^. Sugar.tagReplace) mkPickResult (WidgetIds.tagHoleId (tagId tag))
                    <&> Align.tValue %~ Widget.weakerEvents leaveEventMap
                    <&> (,) TagHole
                | otherwise -> pure (SimpleView, nameView)
                where
                    leaveEventMap =
                        E.keysEventMapMovesCursor
                        (env ^.
                            has . Config.completion . Config.completionCloseKeys)
                        (E.toDoc env
                            [ has . MomentuTexts.navigation
                            , has . Texts.closeHole
                            ])
                        (pure myId)
    & GuiState.assignCursor myId viewId
    where
        myId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        viewId = tagViewId myId
        mkPickResult tagInstance () =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId tagInstance
            , Menu._pickMNextEntry = onPickNext tagInstance
            }
        chooseAction =
            case tag ^. Sugar.tagReplace . Sugar.tsAnon of
            Nothing -> pure myId
            Just setAnon -> setAnon <&> fst <&> WidgetIds.fromEntityId
            <&> WidgetIds.tagHoleId

makeRecordTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TextWidget o)
makeRecordTag =
    makeTagEdit <&> Styled.withColor TextColors.recordTagColor

makeVariantTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TextWidget o)
makeVariantTag tag =
    makeTagEdit tag
    & Styled.withColor TextColors.caseTagColor

addParamId :: Widget.Id -> Widget.Id
addParamId = (`Widget.joinId` ["add param"])

makeLHSTag ::
    ( Monad i, Applicative o
    , Glue.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    (Sugar.EntityId -> Maybe Widget.Id) ->
    Lens.ALens' TextColors Draw.Color -> Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TextWidget o)
makeLHSTag onPickNext color tag =
    do
        env <- Lens.view id
        (tagEditType, tagEdit) <-
            makeTagEditWith onView onPickNext tag
            & Styled.withColor color
            & Reader.local (has .~ env ^. has . Style.nameAtBinder)
        let chooseEventMap =
                E.charEventMap "Letter"
                (E.toDoc env
                    [has . MomentuTexts.edit, has . Texts.tag, has . MomentuTexts.choose])
                chooseWithChar

        let eventMap =
                case tagEditType of
                SimpleView -> chooseEventMap
                _ -> mempty
        tagEdit <&> Widget.weakerEvents eventMap & pure
    where
        chooseWithChar c =
            SearchMenu.enterWithSearchTerm (Text.singleton c)
            (WidgetIds.tagHoleId myId)
            <$ guard (Char.isAlpha c)
            <&> pure
        myId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        -- Apply the name style only when the tag is a view. If it is
        -- a tag hole, the name style (indicating auto-name) makes no sense
        onView =
            Styled.nameAtBinder (tag ^. Sugar.tagInfo . Sugar.tagName) .
            Styled.withColor color

makeParamTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TextWidget o)
makeParamTag =
    makeLHSTag onPickNext TextColors.parameterColor
    where
        onPickNext pos = WidgetIds.fromEntityId pos & addParamId & Just

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag ::
    ( MonadReader env m, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env, Has (Texts.Name Text) env
    ) =>
    Name f -> Sugar.EntityId -> m (WithTextPos View)
makeArgTag name tagInstance =
    NameView.make name
    & Styled.withColor TextColors.argTagColor
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId tagInstance & Widget.toAnimId

makeBinderTagEdit ::
    ( Monad i, Applicative o
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Lens.ALens' TextColors Draw.Color -> Sugar.TagRef (Name o) i o ->
    ExprGuiM env i o (TextWidget o)
makeBinderTagEdit color tag =
    makeLHSTag (const Nothing) color tag
    & Reader.local (has . Menu.configKeys . Menu.keysPickOptionAndGotoNext .~ [])
