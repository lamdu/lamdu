{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts, ConstraintKinds #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeCaseTag, makeCaseTagView
    , makeParamTag
    , makeArgTag
    , makeTagHoleEdit, withNameColor
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
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.NameEdit as NameEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
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

makeTagNameEdit ::
    ( MonadReader env m, HasConfig env, TextEdit.HasStyle env
    , GuiState.HasCursor env, Applicative f
    ) =>
    NearestHoles -> Sugar.Tag (Name f) f ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeTagNameEdit nearestHoles tag =
    do
        keys <- Lens.view Config.config <&> Config.menu <&> Menu.keysPickOptionAndGotoNext
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & foldMap
                  (E.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   pure . WidgetIds.fromEntityId)
        NameEdit.makeBareEdit (tag ^. Sugar.tagName) (tagRenameId myId)
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filterChars (/= ',')
            <&> Align.tValue %~ Widget.weakerEvents jumpNextEventMap
            <&> Align.tValue %~ Widget.weakerEvents stopEditingEventMap
    where
        myId = WidgetIds.fromEntityId (tag ^. Sugar.tagInfo . Sugar.tagInstance)
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
    Maybe Sugar.EntityId -> f Menu.PickResult ->
    m (EventMap (f GuiState.Update))
makePickEventMap mNextEntry action =
    Lens.view Config.config <&> Config.menu <&>
    \config ->
    let pickKeys = Menu.keysPickOption config
        jumpNextKeys = Menu.keysPickOptionAndGotoNext config
    in
    case mNextEntry of
    Nothing -> E.keysEventMapMovesCursor (pickKeys <> jumpNextKeys) doc (action <&> (^. Menu.pickDest))
    Just nextEntry ->
        E.keysEventMapMovesCursor pickKeys doc (action <&> (^. Menu.pickDest))
        <> E.keysEventMapMovesCursor jumpNextKeys
            (mkDoc "New and jump to next hole") (action <&> nextEntryDest)
        where
            nextEntryDest pickDest
                | pickDest ^. Menu.pickDestIsEntryPoint = pickDest ^. Menu.pickDest
                | otherwise = WidgetIds.fromEntityId nextEntry
    where
        doc = mkDoc "New"
        mkDoc x = E.Doc ["Edit", "Tag", x]

makeOptions ::
    ( MonadTransaction m f, MonadReader env f, GuiState.HasCursor env
    , HasConfig env, HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Sugar.TagSelection (Name (T m)) (T m) a -> (Sugar.TagInfo -> a -> Menu.PickResult) ->
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
                <&> filter (Lens.anyOf (Sugar.toName . Name.form . Name._Stored . _1) (insensitiveInfixOf searchTerm))
                <&> splitAt resultCount
                <&> _2 %~ not . null
                <&> uncurry Menu.OptionList
                <&> fmap makeOption
    where
        searchTerm = ctx ^. SearchMenu.rSearchTerm
        makeOption opt =
            Menu.Option
            { Menu._oId = optionWId
            , Menu._oRender =
                (Widget.makeFocusableView ?? optionWId <&> fmap)
                <*> NameEdit.makeView (opt ^. Sugar.toName . Name.form)
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
allowedSearchTerm = Text.all Char.isAlphaNum

type HasSearchTermEnv env =
    ( HasTheme env, HasConfig env, GuiState.HasState env
    , TextEdit.HasStyle env, Hover.HasStyle env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    )

type HasTagEditEnv env = (HasSearchTermEnv env, Menu.HasConfig env)

makeHoleSearchTerm ::
    (MonadReader env m, Monad f, HasSearchTermEnv env) =>
    Maybe Sugar.EntityId ->
    Sugar.TagSelection (Name f) f a -> (Sugar.TagInfo -> a -> Menu.PickResult) ->
    Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeHoleSearchTerm mNextEntry tagSelection mkPickResult holeId =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        let newTag =
                do
                    (name, t, selectResult) <- tagSelection ^. Sugar.tsNewTag
                    (name ^. Name.setName) searchTerm
                    mkPickResult t selectResult & pure
        newTagEventMap <- makePickEventMap mNextEntry newTag
        let pickPreEvent =
                Widget.PreEvent
                { Widget._pDesc = "New tag"
                , Widget._pAction = mempty <$ newTag
                , Widget._pTextRemainder = ""
                }
        term <-
            SearchMenu.basicSearchTermEdit holeId allowedSearchTerm
            <&> Align.tValue . Lens.mapped %~ pure
            <&> Align.tValue %~ Widget.weakerEvents newTagEventMap
            <&> Align.tValue . Widget.wState . Widget._StateFocused .
                Lens.mapped . Widget.fPreEvents %~ (Widget.PreEvents [pickPreEvent] <>)
        tooltip <- Lens.view theme <&> Theme.tooltip
        if not (Text.null searchTerm) && Widget.isFocused (term ^. Align.tValue)
            then
                do
                    newTagLabel <- (TextView.make ?? "(new tag)") <*> Element.subAnimId ["label"]
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
    Maybe Sugar.EntityId ->
    Sugar.TagSelection (Name (T f)) (T f) a -> (Sugar.TagInfo -> a -> Menu.PickResult) ->
    Widget.Id ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeTagHoleEdit mNextEntry tagSelection mkPickResult holeId =
    do
        searchTermEventMap <- SearchMenu.searchTermEditEventMap holeId allowedSearchTerm <&> fmap pure
        SearchMenu.make
            (const (makeHoleSearchTerm mNextEntry tagSelection mkPickResult holeId))
            (makeOptions tagSelection mkPickResult) Element.empty
            (mNextEntry <&> WidgetIds.fromEntityId) holeId
            ?? Menu.AnyPlace
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (<> searchTermEventMap)

makeTagView ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env) =>
    Sugar.Tag (Name f) g -> m (WithTextPos View)
makeTagView tag =
    NameEdit.makeView (tag ^. Sugar.tagName . Name.form)
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
makeTagEdit nearestHoles tag =
    do
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        isHole <- GuiState.isSubCursor ?? WidgetIds.tagHoleId myId
        config <- Lens.view Config.config
        let eventMap =
                E.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
                (E.Doc ["Edit", "Tag", "Open"]) (pure (tagRenameId myId))
                <>
                E.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Edit", "Tag", "Choose"])
                (pure (WidgetIds.tagHoleId myId))
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            makeTagView tag
            <&> Lens.mapped %~ Widget.weakerEvents eventMap
        let hover = Hover.hoverBeside Align.tValue ?? nameView
        widget <-
            if isRenaming
            then
                hover <*>
                (makeTagNameEdit nearestHoles tag <&> (^. Align.tValue))
            else if isHole
            then makeTagHoleEdit (nearestHoles ^. NearestHoles.next) (tag ^. Sugar.tagSelection) mkPickResult (WidgetIds.tagHoleId (tagId tag))
            else pure nameView
        widget
            <&> Widget.weakerEvents jumpHolesEventMap
            & pure
    & GuiState.assignCursor myId viewId
    where
        myId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        viewId = tagViewId myId
        mkPickResult tagInfo () =
            Menu.PickResult
            { Menu._pickDest = tagInfo ^. Sugar.tagInstance & WidgetIds.fromEntityId
            , Menu._pickDestIsEntryPoint = False
            }

withNameColor ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env) =>
    (Theme.Name -> Draw.Color) -> m a -> m a
withNameColor nameColor act =
    do
        color <- Lens.view Theme.theme <&> Theme.name <&> nameColor
        Reader.local (TextView.color .~ color) act

makeRecordTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeRecordTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & withNameColor Theme.recordTagColor

makeCaseTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    NearestHoles -> Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeCaseTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & withNameColor Theme.caseTagColor

makeCaseTagView ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env) =>
    Sugar.Tag (Name g) h -> m (WithTextPos View)
makeCaseTagView = withNameColor Theme.caseTagColor . makeTagView

makeParamTag ::
    (MonadReader env m, MonadTransaction f m, HasTagEditEnv env) =>
    Sugar.Tag (Name (T f)) (T f) ->
    m (WithTextPos (Widget (T f GuiState.Update)))
makeParamTag tag =
    makeTagEdit NearestHoles.none tag
    & withNameColor Theme.parameterColor
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

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Name f -> Sugar.EntityId -> m (WithTextPos View)
makeArgTag name tagInstance =
    do
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        NameEdit.makeView (name ^. Name.form)
            & Reader.local (TextView.color .~ Theme.paramTagColor nameTheme)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId tagInstance & Widget.toAnimId
