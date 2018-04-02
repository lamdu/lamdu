{-# LANGUAGE FlexibleContexts, ConstraintKinds, RankNTypes #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeVariantTag, makeTagView
    , makeParamTag, addParamId
    , makeArgTag
    , makeTagHoleEdit
    , makeBinderTagEdit
    , HasTagEditEnv
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Char as Char
import           Data.MRUMemo (memo)
import           Data.Property (Property(..))
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
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
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import           Lamdu.Style (HasStyle)
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

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
    m (WithTextPos (Widget (f GuiState.Update)))
makeTagNameEdit nearestHoles (Name.StoredName setName tagText _tagCollision storedText) myId =
    do
        keys <- Lens.view (Config.config . Config.menu . Menu.keysPickOptionAndGotoNext)
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & foldMap
                  (E.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   pure . WidgetIds.fromEntityId)
        TextEdits.makeWordEdit
            ?? TextEdit.EmptyStrings (tagText ^. Name.ttText) ""
            ?? Property storedText setName
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

tagId :: Sugar.Tag name im am -> Widget.Id
tagId tag = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId

makePickEventMap ::
    (Functor f, Config.HasConfig env, MonadReader env m) =>
    f Menu.PickResult ->
    m (EventMap (f GuiState.Update))
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
    Monad am =>
    Text -> Sugar.TagSelection (Name am) im am a ->
    (EntityId -> a -> b) -> am b
makeNewTag searchTerm tagSelection mkPickResult =
    do
        (tagInstance, selectResult) <- searchTerm & tagSelection ^. Sugar.tsNewTag
        mkPickResult tagInstance selectResult & pure

makeNewTagPreEvent ::
    Monad am =>
    Text -> Sugar.TagSelection (Name am) im am a ->
    (EntityId -> a -> r) -> Maybe (Widget.PreEvent (am r))
makeNewTagPreEvent searchTerm tagSelection mkPickResult
    | Text.null searchTerm = Nothing
    | otherwise =
        Just Widget.PreEvent
        { Widget._pDesc = "New name"
        , Widget._pAction = makeNewTag searchTerm tagSelection mkPickResult
        , Widget._pTextRemainder = ""
        }

addNewTag ::
    ( Monad m, MonadReader env f, GuiState.HasCursor env, HasTheme env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    ) =>
    Sugar.TagSelection (Name (T m)) (T m) (T m) a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    Maybe (Menu.Option f (T m))
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
    ( MonadTransaction m f, MonadReader env f, GuiState.HasCursor env
    , HasConfig env, HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Sugar.TagSelection (Name (T m)) (T m) (T m) a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    f (Menu.OptionList (Menu.Option f (T m)))
makeOptions tagSelection mkPickResult ctx
    | Text.null searchTerm = pure mempty
    | otherwise =
        do
            resultCount <-
                Lens.view
                (Config.config . Config.completion . Config.completionResultCount)
            results <-
                tagSelection ^. Sugar.tsOptions & transaction
                <&> concatMap withText
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
            let nonFuzzyResults =
                    results ^? Lens.ix 0 . Lens._1 . Fuzzy.isFuzzy
                    & maybe False not
            let maybeAddNewTagOption
                    | nonFuzzyResults = id
                    | otherwise = maybe id (:) (addNewTag tagSelection mkPickResult ctx)
            results <&> snd
                & splitAt resultCount
                & _2 %~ not . null
                & uncurry Menu.OptionList
                <&> makeOption
                & Menu.olOptions %~ maybeAddNewTagOption
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
allowedSearchTerm x =
    Text.all Char.isAlphaNum x
    || Text.all (`elem` Chars.operator) x

type HasSearchTermEnv env =
    ( HasTheme env, HasConfig env, GuiState.HasState env
    , TextEdit.HasStyle env, Hover.HasStyle env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    )

type HasTagEditEnv env = (HasSearchTermEnv env, Menu.HasConfig env)

makeHoleSearchTerm ::
    (MonadReader env m, Monad am, HasSearchTermEnv env) =>
    Sugar.TagSelection (Name am) im am a ->
    (EntityId -> a -> Menu.PickResult) -> Widget.Id ->
    m (WithTextPos (Widget (am GuiState.Update)))
makeHoleSearchTerm tagSelection mkPickResult holeId =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        newTagEventMap <-
            if Text.null searchTerm
            then pure mempty
            else makeNewTag searchTerm tagSelection mkPickResult & makePickEventMap
        let newTagPreEvents =
                makeNewTagPreEvent searchTerm tagSelection mkPickResult
                ^.. Lens._Just
                <&> fmap (mempty <$)
        term <-
            SearchMenu.basicSearchTermEdit holeId allowedSearchTerm
            <&> Align.tValue . Lens.mapped %~ pure
            <&> Align.tValue %~ Widget.weakerEvents newTagEventMap
            <&> Align.tValue . Widget.wState . Widget._StateFocused .
                Lens.mapped . Widget.fPreEvents %~
                (Widget.PreEvents newTagPreEvents <>)
        tooltip <- Lens.view (theme . Theme.tooltip)
        if not (Text.null searchTerm) && Widget.isFocused (term ^. Align.tValue)
            then
                do
                    newTagLabel <-
                        (TextView.make ?? "(new)") <*> Element.subAnimId ["label"]
                    space <- Spacer.stdHSpace
                    hover <- Hover.hover
                    let anchor = fmap Hover.anchor
                    let hNewTagLabel = hover newTagLabel & Hover.sequenceHover
                    let hoverOptions =
                            [ anchor (term /|/ space) /|/ hNewTagLabel
                            , hNewTagLabel /|/ anchor (space /|/ term)
                            ] <&> (^. Align.tValue)
                    anchor term
                        <&> Hover.hoverInPlaceOf hoverOptions
                        & pure
                    & Reader.local (Hover.backgroundColor .~ tooltip ^. Theme.tooltipBgColor)
                    & Reader.local (TextView.color .~ tooltip ^. Theme.tooltipFgColor)
                    & Reader.local (Element.animIdPrefix <>~ ["label"])
            else pure term

makeTagHoleEdit ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    Sugar.TagSelection (Name (T f)) (T f) (T f) a ->
    (EntityId -> a -> Menu.PickResult) ->
    Widget.Id ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeTagHoleEdit tagSelection mkPickResult holeId =
    do
        searchTermEventMap <- SearchMenu.searchTermEditEventMap holeId allowedSearchTerm <&> fmap pure
        SearchMenu.make
            (const (makeHoleSearchTerm tagSelection mkPickResult holeId))
            (makeOptions tagSelection mkPickResult) Element.empty holeId
            ?? Menu.AnyPlace
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (<> searchTermEventMap)

makeTagView ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env) =>
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
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeTagEdit = makeTagEditWith id defaultOnPickNext <&> (fmap . fmap) snd

defaultOnPickNext :: Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id
defaultOnPickNext mNextEntry pos = fromMaybe pos mNextEntry & WidgetIds.fromEntityId

data TagEditType
    = TagRename
    | TagHole
    | SimpleView
    deriving (Eq)

makeTagEditWith ::
    ( MonadReader env m, MonadReader env n, MonadTransaction f m
    , HasTagEditEnv env
    ) =>
    (n (WithTextPos (Widget (T f GuiState.Update))) ->
     m (WithTextPos (Widget (T f GuiState.Update)))) ->
    (Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id) ->
    NearestHoles ->
    Sugar.Tag (Name (T f)) (T f) (T f) ->
    m (TagEditType, WithTextPos (Widget (T f GuiState.Update)))
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
                    <&> (,) TagHole
                | otherwise -> pure (SimpleView, nameView)
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
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeRecordTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & Styled.withColor TextColors.recordTagColor

makeVariantTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeVariantTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & Styled.withColor TextColors.caseTagColor

addParamId :: Widget.Id -> Widget.Id
addParamId = (`Widget.joinId` ["add param"])

makeLHSTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env, HasStyle env) =>
    (Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id) ->
    Lens.Getter TextColors Draw.Color -> Sugar.Tag (Name (T f)) (T f) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeLHSTag onPickNext color tag =
    do
        style <- Lens.view Style.style
        (tagEditType, tagEdit) <-
            makeTagEditWith onView onPickNext NearestHoles.none tag
            & Styled.withColor color
            & Reader.local (TextEdit.style .~ style ^. Style.styleNameAtBinder)
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
        onView = Styled.nameAtBinder color (tag ^. Sugar.tagInfo . Sugar.tagName)

makeParamTag ::
    ( MonadReader env f, HasTheme env, HasConfig env, HasStyle env
    , Hover.HasStyle env, Menu.HasConfig env
    , GuiState.HasState env, Element.HasAnimIdPrefix env
    , HasStdSpacing env, MonadTransaction m f
    ) =>
    Sugar.Tag (Name (T m)) (T m) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
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
    ( MonadReader env m, HasTheme env, HasStyle env, HasConfig env
    , GuiState.HasState env
    , Element.HasAnimIdPrefix env, HasStdSpacing env
    , Hover.HasStyle env, Menu.HasConfig env
    , MonadTransaction f m
    ) =>
    Lens.Getter TextColors Draw.Color -> Sugar.Tag (Name (T f)) (T f) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeBinderTagEdit color tag =
    makeLHSTag defaultOnPickNext color tag
    & Reader.local (Menu.config . Menu.configKeys . Menu.keysPickOptionAndGotoNext .~ [])
