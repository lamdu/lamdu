{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeCaseTag, makeCaseTagView
    , makeParamTag
    , makeArgTag
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Char as Char
import           Data.Function (on)
import           Data.Store.Transaction (Transaction)
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
                & maybe mempty
                  (E.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   pure . WidgetIds.fromEntityId)
        NameEdit.makeBareEdit
            (tag ^. Sugar.tagName & Name.setName .~ tag ^. Sugar.tagName . Name.setName)
            (tagRenameId myId)
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
    NearestHoles -> E.Doc -> f Widget.Id ->
    m (EventMap (f GuiState.Update))
makePickEventMap nearestHoles doc action =
    Lens.view Config.config <&> Config.menu <&>
    \config ->
    let pickKeys = Menu.keysPickOption config
        jumpNextKeys = Menu.keysPickOptionAndGotoNext config
    in
    case nearestHoles ^. NearestHoles.next of
    Nothing -> E.keysEventMapMovesCursor (pickKeys <> jumpNextKeys) doc action
    Just nextHole ->
        E.keysEventMapMovesCursor pickKeys doc action
        <> E.keysEventMapMovesCursor jumpNextKeys
            (doc & E.docStrs . Lens.reversed . Lens.ix 0 %~ (<> " and jump to next hole"))
            (WidgetIds.fromEntityId nextHole <$ action)

makeOptions ::
    ( Monad m, MonadTransaction m f, MonadReader env f, GuiState.HasCursor env
    , HasConfig env, HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Sugar.Tag (Name n) (T m) -> SearchMenu.ResultsContext ->
    f (Menu.OptionList (Menu.Option f (T m)))
makeOptions tag ctx
    | Text.null searchTerm = pure mempty
    | otherwise =
        do
            resultCount <-
                Lens.view Config.config
                <&> Config.completion <&> Config.completionResultCount
            tag ^. Sugar.tagSelection . Sugar.tsOptions & transaction
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
                    , Widget._pAction =
                        Menu.PickResult
                        { Menu._pickDest = instanceId
                        , Menu._pickDestIsEntryPoint = False
                        } <$
                        (tag ^. Sugar.tagSelection . Sugar.tsSetTag) (info ^. Sugar.tagVal)
                    , Widget._pTextRemainder = ""
                    }
                }
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
            }
            where
                info = opt ^. Sugar.toInfo
                instanceId = info ^. Sugar.tagInstance & WidgetIds.fromEntityId
                optionWId = ctx ^. SearchMenu.rResultIdPrefix <> instanceId

allowedSearchTerm :: Text -> Bool
allowedSearchTerm = Text.all Char.isAlphaNum

makeHoleSearchTerm ::
    ( MonadReader env m, GuiState.HasState env, HasConfig env, TextEdit.HasStyle env
    , HasTheme env, Element.HasAnimIdPrefix env, HasStdSpacing env, Hover.HasStyle env
    , Monad f
    ) =>
    NearestHoles -> Sugar.Tag (Name f) f ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeHoleSearchTerm nearestHoles tag =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        let newTag =
                do
                    (name, t) <- tag ^. Sugar.tagSelection . Sugar.tsNewTag
                    (name ^. Name.setName) searchTerm
                    t ^. Sugar.tagInstance & pure
        newTagEventMap <-
            newTag <&> WidgetIds.fromEntityId
            & makePickEventMap nearestHoles (E.Doc ["Edit", "Tag", "New"])
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
    where
        holeId = WidgetIds.tagHoleId (tagId tag)

makeTagHoleEdit ::
    ( Monad m, MonadReader env f, MonadTransaction m f
    , GuiState.HasState env
    , HasConfig env, TextEdit.HasStyle env, Element.HasAnimIdPrefix env
    , HasTheme env, Hover.HasStyle env, Menu.HasConfig env, HasStdSpacing env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeTagHoleEdit nearestHoles tag =
    do
        searchTermEventMap <- SearchMenu.searchTermEditEventMap holeId allowedSearchTerm <&> fmap pure
        SearchMenu.make (const (makeHoleSearchTerm nearestHoles tag)) (makeOptions tag) Element.empty
            (nearestHoles ^. NearestHoles.next <&> WidgetIds.fromEntityId) holeId
            ?? Menu.AnyPlace
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (<> searchTermEventMap)
    where
        holeId = WidgetIds.tagHoleId (tagId tag)

makeTagView ::
    ( MonadReader env m
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env
    ) =>
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
    ( Monad m, MonadReader env f, MonadTransaction m f, HasConfig env
    , GuiState.HasState env, HasTheme env, Element.HasAnimIdPrefix env
    , TextEdit.HasStyle env, Hover.HasStyle env, Menu.HasConfig env
    , HasStdSpacing env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
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
            then makeTagHoleEdit nearestHoles tag
            else pure nameView
        widget
            <&> Widget.weakerEvents jumpHolesEventMap
            & pure
    & GuiState.assignCursor myId viewId
    where
        myId = WidgetIds.fromEntityId (tag ^. Sugar.tagInfo . Sugar.tagInstance)
        viewId = tagViewId myId

withNameColor ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env) =>
    (Theme.Name -> Draw.Color) -> m a -> m a
withNameColor nameColor act =
    do
        color <- Lens.view Theme.theme <&> Theme.name <&> nameColor
        Reader.local (TextView.color .~ color) act

makeRecordTag ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasTheme env
    , HasConfig env, GuiState.HasState env
    , Element.HasAnimIdPrefix env, HasStdSpacing env
    , TextEdit.HasStyle env, Hover.HasStyle env, HasTheme env, Menu.HasConfig env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeRecordTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & withNameColor Theme.recordTagColor

makeCaseTag ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasTheme env
    , HasConfig env, GuiState.HasState env
    , TextEdit.HasStyle env, Hover.HasStyle env, HasTheme env, Menu.HasConfig env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeCaseTag nearestHoles tag =
    makeTagEdit nearestHoles tag
    & withNameColor Theme.caseTagColor

makeCaseTagView ::
    ( MonadReader env f
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, HasTheme env
    ) =>
    Sugar.Tag (Name g) h -> f (WithTextPos View)
makeCaseTagView = withNameColor Theme.caseTagColor . makeTagView

makeParamTag ::
    ( MonadReader env f, HasTheme env, HasConfig env, Hover.HasStyle env, Menu.HasConfig env
    , GuiState.HasState env, Element.HasAnimIdPrefix env
    , TextEdit.HasStyle env, HasStdSpacing env, MonadTransaction m f
    ) =>
    Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
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
