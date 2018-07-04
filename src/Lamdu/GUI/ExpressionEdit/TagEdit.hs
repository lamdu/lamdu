{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
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
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
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
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

tagRenameId :: Widget.Id -> Widget.Id
tagRenameId = (`Widget.joinId` ["rename"])

tagViewId :: Widget.Id -> Widget.Id
tagViewId = (`Widget.joinId` ["view"])

disallowedNameChars :: String
disallowedNameChars = ",[]\\`()"

makeTagNameEdit ::
    ( MonadReader env m, HasConfig env, TextEdit.HasStyle env
    , GuiState.HasCursor env, Applicative f
    ) =>
    NearestHoles -> Name.StoredName f -> Widget.Id ->
    m (TextWidget f)
makeTagNameEdit nearestHoles (Name.StoredName prop tagText _tagCollision) myId =
    do
        keys <- Lens.view (Config.config . Config.menu . Menu.keysPickOptionAndGotoNext)
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & foldMap
                  (E.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   pure . WidgetIds.fromEntityId)
        TextEdits.makeWordEdit
            ?? TextEdit.Modes
                { TextEdit._unfocused = tagText ^. Name.ttText
                , TextEdit._focused = ""
                }
            ?? prop
            ?? tagRenameId myId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (`notElem`disallowedNameChars)
            <&> Align.tValue %~ Widget.weakerEvents jumpNextEventMap
            <&> Align.tValue %~ Widget.weakerEvents stopEditingEventMap
    where
        stopEditingEventMap =
            E.keysEventMapMovesCursor
            [ MetaKey noMods MetaKey.Key'Escape
            , MetaKey noMods MetaKey.Key'Enter
            ]
            (E.Doc ["Edit", "Tag", "Stop editing"]) (pure (tagViewId myId))

tagId :: Sugar.Tag name i o -> Widget.Id
tagId tag = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId

makePickEventMap ::
    (Functor f, Config.HasConfig env, MonadReader env m) =>
    f Menu.PickResult ->
    m (Gui EventMap f)
makePickEventMap action =
    Lens.view (Config.config . Config.menu) <&>
    \config ->
    let pickKeys = config ^. Menu.keysPickOption
        jumpNextKeys = config ^. Menu.keysPickOptionAndGotoNext
    in
    E.keysEventMapMovesCursor pickKeys doc (action <&> (^. Menu.pickDest))
    <> E.keysEventMapMovesCursor jumpNextKeys
        (mkDoc "New and jump to next hole") (action <&> (^. Menu.pickNextEntryPoint))
    where
        doc = mkDoc "New"
        mkDoc x = E.Doc ["Edit", "Tag", x]

makeNewTag ::
    Functor o =>
    Text -> Sugar.TagSelection (Name o) i o a ->
    (EntityId -> a -> b) -> o b
makeNewTag searchTerm tagSelection mkPickResult =
    (tagSelection ^. Sugar.tsNewTag) searchTerm <&> uncurry mkPickResult

makeNewTagPreEvent ::
    Functor o =>
    Text -> Sugar.TagSelection (Name o) i o a ->
    (EntityId -> a -> r) -> Maybe (Widget.PreEvent (o r))
makeNewTagPreEvent searchTerm tagSelection mkPickResult
    | Text.null searchTerm = Nothing
    | otherwise =
        Just Widget.PreEvent
        { Widget._pDesc = "New name"
        , Widget._pAction = makeNewTag searchTerm tagSelection mkPickResult
        , Widget._pTextRemainder = ""
        }

addNewTag ::
    ( Applicative o, MonadReader env f, GuiState.HasCursor env, HasTheme env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    ) =>
    Sugar.TagSelection (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    Maybe (Menu.Option f o)
addNewTag tagSelection mkPickResult ctx =
    makeNewTagPreEvent searchTerm tagSelection mkPickResult
    <&> \preEvent ->
    Menu.Option
    { Menu._oId = optionId
    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
    , Menu._oRender =
        (Widget.makeFocusableView ?? optionId <&> fmap)
        <*> TextView.makeLabel "Create new"
        <&> (`Menu.RenderedOption` preEvent)
        & Styled.withColor TextColors.actionTextColor
    }
    where
        optionId = (ctx ^. SearchMenu.rResultIdPrefix) `Widget.joinId` ["Create new"]
        searchTerm = ctx ^. SearchMenu.rSearchTerm

nameText :: Lens.Traversal' (Sugar.TagOption (Name f) m a) Text
nameText = Sugar.toInfo . Sugar.tagName . Name._Stored . Name.snDisplayText . Name.ttText

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

makeOptions ::
    ( Monad i, Applicative o, MonadReader env m
    , GuiState.HasCursor env, HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    Sugar.TagSelection (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    ExprGuiM i o (Menu.OptionList (Menu.Option m o))
makeOptions tagSelection mkPickResult ctx
    | Text.null searchTerm = pure Menu.TooMany
    | otherwise =
        do
            resultCount <-
                Lens.view
                (Config.config . Config.completion . Config.completionResultCount)
            results <-
                tagSelection ^. Sugar.tsOptions
                <&> concatMap withText
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                & ExprGuiM.im
            let nonFuzzyResults =
                    results ^? Lens.ix 0 . Lens._1 . Fuzzy.isFuzzy
                    & maybe False not
            let maybeAddNewTagOption
                    | nonFuzzyResults || not (allowedTagName searchTerm) = id
                    | otherwise = maybe id (:) (addNewTag tagSelection mkPickResult ctx)
            results <&> snd
                & splitAt resultCount
                & _2 %~ not . null
                & _1 %~ maybeAddNewTagOption . map makeOption
                & uncurry Menu.toOptionList
                & pure
    where
        withText tagOption = tagOption ^.. nameText <&> ((,) ?? tagOption)
        searchTerm = ctx ^. SearchMenu.rSearchTerm
        makeOption opt =
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
                    { Widget._pDesc = "Pick"
                    , Widget._pAction = opt ^. Sugar.toPick <&> mkPickResult (opt ^. Sugar.toInfo . Sugar.tagInstance)
                    , Widget._pTextRemainder = ""
                    }
                }
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
            }
            where
                instanceId = opt ^. Sugar.toInfo . Sugar.tagInstance & WidgetIds.fromEntityId
                optionWId = ctx ^. SearchMenu.rResultIdPrefix <> instanceId

allowedSearchTerm :: Text -> Bool
allowedSearchTerm = Name.isValidText

-- Allowed name for tag assuming it is already a valid search term
allowedTagName :: Text -> Bool
allowedTagName =
    Lens.anyOf (Lens.ix 0) f
    where
        f x = Char.isAlpha x || elem x Chars.operator

type HasSearchTermEnv env =
    ( HasTheme env, HasConfig env, GuiState.HasState env
    , TextEdit.HasStyle env, Hover.HasStyle env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    )

makeHoleSearchTerm ::
    (MonadReader env m, Applicative o, HasSearchTermEnv env) =>
    Sugar.TagSelection (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) -> Widget.Id ->
    m (SearchMenu.Term o)
makeHoleSearchTerm tagSelection mkPickResult holeId =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        let allowNewTag = allowedTagName searchTerm
        newTagEventMap <-
            if allowNewTag
            then makeNewTag searchTerm tagSelection mkPickResult & makePickEventMap
            else pure mempty
        let newTagPreEvents =
                makeNewTagPreEvent searchTerm tagSelection mkPickResult
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
        tooltip <- Lens.view (theme . Theme.tooltip)
        if  allowNewTag &&
            Widget.isFocused (term ^. SearchMenu.termWidget . Align.tValue)
            then
                do
                    newTagLabel <-
                        (TextView.make ?? "(new)") <*> Element.subAnimId ["label"]
                    space <- Spacer.stdHSpace
                    hover <- Hover.hover
                    let hNewTagLabel = hover newTagLabel & Hover.sequenceHover
                    let termWithHover termW =
                            let hoverOptions =
                                    [ anchor (termW /|/ space) /|/ hNewTagLabel
                                    , hNewTagLabel /|/ anchor (space /|/ termW)
                                    ] <&> (^. Align.tValue)
                            in  anchor termW
                                <&> Hover.hoverInPlaceOf hoverOptions
                    term & SearchMenu.termWidget %~ termWithHover & pure
                    & Reader.local (Hover.backgroundColor .~ tooltip ^. Theme.tooltipBgColor)
                    & Reader.local (TextView.color .~ tooltip ^. Theme.tooltipFgColor)
                    & Reader.local (Element.animIdPrefix <>~ ["label"])
            else pure term
    where
        anchor = fmap Hover.anchor

makeTagHoleEdit ::
    (Monad i, Applicative o) =>
    Sugar.TagSelection (Name o) i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    Widget.Id ->
    ExprGuiM i o (TextWidget o)
makeTagHoleEdit tagSelection mkPickResult holeId =
    SearchMenu.make
    (const (makeHoleSearchTerm tagSelection mkPickResult holeId))
    (makeOptions tagSelection mkPickResult) Element.empty holeId
    ?? Menu.AnyPlace

makeTagView ::
    ( MonadReader env m, TextView.HasStyle env
    , Element.HasAnimIdPrefix env, HasTheme env
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
    (Monad i, Monad o) =>
    NearestHoles -> Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TextWidget o)
makeTagEdit = makeTagEditWith id defaultOnPickNext <&> (fmap . fmap) snd

defaultOnPickNext :: Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id
defaultOnPickNext mNextEntry pos = fromMaybe pos mNextEntry & WidgetIds.fromEntityId

data TagEditType
    = TagRename
    | TagHole
    | SimpleView
    deriving (Eq)

makeTagEditWith ::
    ( Monad i, Applicative o, MonadReader env n
    , GuiState.HasCursor env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env, HasTheme env
    ) =>
    (n (TextWidget o) ->
     ExprGuiM i o (TextWidget o)) ->
    (Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id) ->
    NearestHoles ->
    Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TagEditType, TextWidget o)
makeTagEditWith onView onPickNext nearestHoles tag =
    do
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        let mRenamingStoredName
                | isRenaming = tag ^? Sugar.tagInfo . Sugar.tagName . Name._Stored
                | otherwise = Nothing
        isHole <- GuiState.isSubCursor ?? WidgetIds.tagHoleId myId
        config <- Lens.view Config.config
        let eventMap =
                ( case tag ^. Sugar.tagInfo . Sugar.tagName of
                    Name.Stored{} ->
                        E.keysEventMapMovesCursor (config ^. Config.jumpToDefinitionKeys)
                        (E.Doc ["Edit", "Tag", "Rename tag"]) (pure (tagRenameId myId))
                    _ -> mempty
                )
                <>
                E.keysEventMapMovesCursor
                (Config.delKeys config <> config ^. Config.jumpToDefinitionKeys)
                (E.Doc ["Edit", "Tag", "Choose"]) chooseAction
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            makeTagView (tag ^. Sugar.tagInfo)
            <&> Lens.mapped %~ Widget.weakerEvents eventMap
            & onView
        let hover = Hover.hoverBeside Align.tValue ?? nameView
        (tagEditType, widget) <-
            case mRenamingStoredName of
            Just storedName ->
                hover <*>
                (makeTagNameEdit nearestHoles storedName myId <&> (^. Align.tValue))
                <&> (,) TagRename
            Nothing
                | isHole ->
                    makeTagHoleEdit (tag ^. Sugar.tagSelection) mkPickResult (WidgetIds.tagHoleId (tagId tag))
                    <&> Align.tValue %~ Widget.weakerEvents leaveEventMap
                    <&> (,) TagHole
                | otherwise -> pure (SimpleView, nameView)
                where
                    leaveEventMap =
                        E.keysEventMapMovesCursor
                        (config ^. Config.completion . Config.completionCloseKeys)
                        (E.Doc ["Navigation", "Close  hole"])
                        (pure myId)
        pure (tagEditType, widget <&> Widget.weakerEvents jumpHolesEventMap)
    & GuiState.assignCursor myId viewId
    where
        myId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        viewId = tagViewId myId
        mkPickResult tagInstance () =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId tagInstance
            , Menu._pickNextEntryPoint =
                onPickNext (nearestHoles ^. NearestHoles.next) tagInstance
            }
        chooseAction =
            case tag ^. Sugar.tagSelection . Sugar.tsAnon of
            Nothing -> pure myId
            Just setAnon -> setAnon <&> fst <&> WidgetIds.fromEntityId
            <&> WidgetIds.tagHoleId

makeRecordTag ::
    (Monad i, Monad o) =>
    NearestHoles -> Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TextWidget o)
makeRecordTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & Styled.withColor TextColors.recordTagColor

makeVariantTag ::
    (Monad i, Monad o) =>
    NearestHoles -> Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TextWidget o)
makeVariantTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & Styled.withColor TextColors.caseTagColor

addParamId :: Widget.Id -> Widget.Id
addParamId = (`Widget.joinId` ["add param"])

makeLHSTag ::
    (Monad i, Applicative o) =>
    (Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id) ->
    Lens.ALens' TextColors Draw.Color -> Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TextWidget o)
makeLHSTag onPickNext color tag =
    do
        style <- Lens.view Style.style
        (tagEditType, tagEdit) <-
            makeTagEditWith onView onPickNext NearestHoles.none tag
            & Styled.withColor color
            & Reader.local (TextEdit.style .~ style ^. Style.nameAtBinder)
        let eventMap =
                case tagEditType of
                SimpleView -> chooseEventMap
                _ -> mempty
        tagEdit <&> Widget.weakerEvents eventMap & pure
    where
        chooseEventMap =
            E.charEventMap "Character" (E.Doc ["Edit", "Tag", "Choose"]) chooseWithChar
        chooseWithChar c =
            pure (SearchMenu.enterWithSearchTerm (Text.singleton c) (WidgetIds.tagHoleId myId))
            <$ guard (Char.isAlpha c)
        myId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        -- Apply the name style only when the tag is a view. If it is
        -- a tag hole, the name style (indicating auto-name) makes no sense
        onView =
            Styled.nameAtBinder (tag ^. Sugar.tagInfo . Sugar.tagName) .
            Styled.withColor color

makeParamTag ::
    (Monad i, Monad o) =>
    Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TextWidget o)
makeParamTag =
    makeLHSTag onPickNext TextColors.parameterColor
    where
        onPickNext _ pos = WidgetIds.fromEntityId pos & addParamId

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Name f -> Sugar.EntityId -> m (WithTextPos View)
makeArgTag name tagInstance =
    NameView.make name
    & Styled.withColor TextColors.argTagColor
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId tagInstance & Widget.toAnimId

makeBinderTagEdit ::
    (Monad i, Applicative o) =>
    Lens.ALens' TextColors Draw.Color -> Sugar.Tag (Name o) i o ->
    ExprGuiM i o (TextWidget o)
makeBinderTagEdit color tag =
    makeLHSTag defaultOnPickNext color tag
    & Reader.local (Menu.config . Menu.configKeys . Menu.keysPickOptionAndGotoNext .~ [])
