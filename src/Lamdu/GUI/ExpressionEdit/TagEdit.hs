{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts, ConstraintKinds #-}
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
import           Data.Function (on)
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
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import           Lamdu.Style (HasStyle)
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Property (Property(..))
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
        keys <- Lens.view Config.config <&> Config.menu <&> Menu.keysPickOptionAndGotoNext
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

insensitiveInfixOf :: Text -> Text -> Bool
insensitiveInfixOf = Text.isInfixOf `on` Text.toLower

tagId :: Sugar.Tag name m -> Widget.Id
tagId tag = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId

makePickEventMap ::
    (Functor f, Config.HasConfig env, MonadReader env m) =>
    f Menu.PickResult ->
    m (EventMap (f GuiState.Update))
makePickEventMap action =
    Lens.view Config.config <&> Config.menu <&>
    \config ->
    let pickKeys = Menu.keysPickOption config
        jumpNextKeys = Menu.keysPickOptionAndGotoNext config
    in
    E.keysEventMapMovesCursor pickKeys doc (action <&> (^. Menu.pickDest))
    <> E.keysEventMapMovesCursor jumpNextKeys
        (mkDoc "New and jump to next hole") (action <&> (^. Menu.pickNextEntryPoint))
    where
        doc = mkDoc "New"
        mkDoc x = E.Doc ["Edit", "Tag", x]

makeNewTag ::
    Monad m =>
    Text -> Sugar.TagSelection (Name m) m a ->
    (Sugar.TagInfo -> a -> b) -> m b
makeNewTag searchTerm tagSelection mkPickResult =
    do
        (name, t, selectResult) <- tagSelection ^. Sugar.tsNewTag
        case name ^? Name._Stored . Name.snSet of
            Nothing -> pure ()
            Just setName -> setName searchTerm
        mkPickResult t selectResult & pure

makeNewTagPreEvent ::
    Monad f =>
    Text -> Sugar.TagSelection (Name f) f a ->
    (Sugar.TagInfo -> a -> r) -> Maybe (Widget.PreEvent (f r))
makeNewTagPreEvent searchTerm tagSelection mkPickResult
    | Text.null searchTerm = Nothing
    | otherwise =
        Just Widget.PreEvent
        { Widget._pDesc = "New name"
        , Widget._pAction = makeNewTag searchTerm tagSelection mkPickResult
        , Widget._pTextRemainder = ""
        }

addNewTagIfNullOptions ::
    ( Monad m, MonadReader env f, GuiState.HasCursor env, HasTheme env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    ) =>
    Sugar.TagSelection (Name (T m)) (T m) a ->
    (Sugar.TagInfo -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    Menu.OptionList (Menu.Option f (T m)) ->
    Menu.OptionList (Menu.Option f (T m))
addNewTagIfNullOptions tagSelection mkPickResult ctx optionList =
    case makeNewTagPreEvent searchTerm tagSelection mkPickResult of
    Just preEvent
        | null (optionList ^. Menu.olOptions) ->
            Menu.OptionList
            { Menu._olOptions =
                [ Menu.Option
                    { Menu._oId = optionId
                    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
                    , Menu._oRender =
                        do
                            color <- Lens.view theme <&> Theme.actionTextColor
                            (Widget.makeFocusableView ?? optionId <&> fmap)
                                <*> TextView.makeLabel "Create new"
                                <&> (`Menu.RenderedOption` preEvent)
                                & Reader.local (TextView.color .~ color)
                    }
                ]
            , Menu._olIsTruncated = False
            }
    _ -> optionList
    where
        optionId = (ctx ^. SearchMenu.rResultIdPrefix) `Widget.joinId` ["Create new"]
        searchTerm = ctx ^. SearchMenu.rSearchTerm

makeOptions ::
    ( MonadTransaction m f, MonadReader env f, GuiState.HasCursor env
    , HasConfig env, HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Sugar.TagSelection (Name (T m)) (T m) a ->
    (Sugar.TagInfo -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    f (Menu.OptionList (Menu.Option f (T m)))
makeOptions tagSelection mkPickResult ctx
    | Text.null searchTerm = pure mempty
    | otherwise =
        do
            resultCount <-
                Lens.view Config.config
                <&> Config.completion <&> Config.completionResultCount
            tagSelection ^. Sugar.tsOptions & transaction
                <&> filter (Lens.anyOf nameText isFit)
                <&> splitAt resultCount
                <&> _2 %~ not . null
                <&> uncurry Menu.OptionList
                <&> fmap makeOption
                <&> addNewTagIfNullOptions tagSelection mkPickResult ctx
    where
        isFit
            | Text.length searchTerm == 1 = (==) searchTerm
            | otherwise = insensitiveInfixOf searchTerm
        nameText = Sugar.toName . Name._Stored . Name.snDisplayText . Name.ttText
        searchTerm = ctx ^. SearchMenu.rSearchTerm
        makeOption opt =
            Menu.Option
            { Menu._oId = optionWId
            , Menu._oRender =
                (Widget.makeFocusableView ?? optionWId <&> fmap)
                <*> NameView.make (opt ^. Sugar.toName)
                & Reader.local (Element.animIdPrefix .~ Widget.toAnimId instanceId)
                <&>
                \widget ->
                Menu.RenderedOption
                { Menu._rWidget = widget
                , Menu._rPick = Widget.PreEvent
                    { Widget._pDesc = "Pick"
                    , Widget._pAction = opt ^. Sugar.toPick <&> mkPickResult (opt ^. Sugar.toInfo)
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
    (MonadReader env m, Monad f, HasSearchTermEnv env) =>
    Sugar.TagSelection (Name f) f a ->
    (Sugar.TagInfo -> a -> Menu.PickResult) -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
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
        tooltip <- Lens.view theme <&> Theme.tooltip
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
                    & Reader.local (Hover.backgroundColor .~ Theme.tooltipBgColor tooltip)
                    & Reader.local (TextView.color .~ Theme.tooltipFgColor tooltip)
                    & Reader.local (Element.animIdPrefix <>~ ["label"])
            else pure term

makeTagHoleEdit ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    Sugar.TagSelection (Name (T f)) (T f) a -> (Sugar.TagInfo -> a -> Menu.PickResult) ->
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
    Sugar.Tag (Name f) g -> m (WithTextPos View)
makeTagView tag =
    NameView.make (tag ^. Sugar.tagName)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId =
            tag ^. Sugar.tagInfo . Sugar.tagInstance
            & WidgetIds.fromEntityId
            & Widget.toAnimId

makeTagEdit ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) ->
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
    Sugar.Tag (Name (T f)) (T f) ->
    m (TagEditType, WithTextPos (Widget (T f GuiState.Update)))
makeTagEditWith onView onPickNext nearestHoles tag =
    do
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        let mRenamingStoredName
                | isRenaming = tag ^? Sugar.tagName . Name._Stored
                | otherwise = Nothing
        isHole <- GuiState.isSubCursor ?? WidgetIds.tagHoleId myId
        config <- Lens.view Config.config
        let eventMap =
                ( case tag ^. Sugar.tagName of
                    Name.Stored{} ->
                        E.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
                        (E.Doc ["Edit", "Tag", "Rename tag"]) (pure (tagRenameId myId))
                    _ -> mempty
                )
                <>
                E.keysEventMapMovesCursor (Config.delKeys config <> Config.jumpToDefinitionKeys config)
                (E.Doc ["Edit", "Tag", "Choose"]) chooseAction
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            makeTagView tag
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
        mkPickResult tagInfo () =
            Menu.PickResult
            { Menu._pickDest = tagInfo ^. Sugar.tagInstance & WidgetIds.fromEntityId
            , Menu._pickNextEntryPoint =
                onPickNext (nearestHoles ^. NearestHoles.next) (tagInfo ^. Sugar.tagInstance)
            }
        chooseAction =
            case tag ^. Sugar.tagSelection . Sugar.tsAnon of
            Nothing -> pure myId
            Just setAnon -> setAnon <&> fst <&> WidgetIds.fromEntityId
            <&> WidgetIds.tagHoleId

makeRecordTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeRecordTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & Styled.withColor (Theme.recordTagColor . Theme.name)

makeVariantTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeVariantTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & Styled.withColor (Theme.caseTagColor . Theme.name)

addParamId :: Widget.Id -> Widget.Id
addParamId = (`Widget.joinId` ["add param"])

makeLHSTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env, HasStyle env) =>
    (Maybe Sugar.EntityId -> Sugar.EntityId -> Widget.Id) ->
    (Theme.Name -> Draw.Color) -> Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeLHSTag onPickNext color tag =
    do
        style <- Lens.view Style.style
        (tagEditType, tagEdit) <-
            makeTagEditWith onView onPickNext NearestHoles.none tag
            & Styled.withColor (color . Theme.name)
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
        onView = Styled.nameAtBinder color (tag ^. Sugar.tagName)

makeParamTag ::
    ( MonadReader env f, HasTheme env, HasConfig env, HasStyle env
    , Hover.HasStyle env, Menu.HasConfig env
    , GuiState.HasState env, Element.HasAnimIdPrefix env
    , HasStdSpacing env, MonadTransaction m f
    ) =>
    Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeParamTag =
    makeLHSTag onPickNext Theme.parameterColor
    where
        onPickNext _ pos = WidgetIds.fromEntityId pos & addParamId

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Name f -> Sugar.EntityId -> m (WithTextPos View)
makeArgTag name tagInstance =
    do
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        NameView.make name
            & Reader.local (TextView.color .~ Theme.argTagColor nameTheme)
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
    (Theme.Name -> Draw.Color) -> Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeBinderTagEdit color tag =
    makeLHSTag defaultOnPickNext color tag
    & Reader.local (Menu.config %~ removeGoNextKeys)
    where
        removeGoNextKeys c =
            -- Would be nicer with lens but that would make the JSON format ugly..
            c
            { Menu.configKeys =
                (Menu.configKeys c)
                { Menu.keysPickOptionAndGotoNext = []
                }
            }
