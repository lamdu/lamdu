{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeCaseTag
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

setTagName :: Monad m => Sugar.Tag (Name (T m)) (T m) -> Text -> T m ()
setTagName tag name =
    do
        (tag ^. Sugar.tagName . Name.setName) name
        (tag ^. Sugar.tagActions . Sugar.taSetPublished) (not (Text.null name))

makeTagNameEdit ::
    ( Monad m, MonadReader env f, HasConfig env, TextEdit.HasStyle env
    , GuiState.HasCursor env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeTagNameEdit nearestHoles tag =
    do
        keys <- Lens.view Config.config <&> Config.menu <&> Menu.keysPickOptionAndGotoNext
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & maybe mempty
                  (E.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   return . WidgetIds.fromEntityId)
        NameEdit.makeBareEdit
            (tag ^. Sugar.tagName & Name.setName .~ setTagName tag)
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
    Sugar.Tag (Name n) (T m) -> Text ->
    f (Menu.OptionList (Menu.Option f (T m)))
makeOptions tag searchTerm
    | Text.null searchTerm = pure mempty
    | otherwise =
        do
            resultCount <-
                Lens.view Config.config
                <&> Config.hole <&> Config.holeResultCount
            tag ^. Sugar.tagActions . Sugar.taOptions & transaction
                <&> filter (Lens.anyOf (_1 . Name.form . Name._Stored . _1) (insensitiveInfixOf searchTerm))
                <&> splitAt resultCount
                <&> _2 %~ not . null
                <&> uncurry Menu.OptionList
                <&> fmap makeOption
    where
        makeOption (name, t) =
            Menu.Option
            { Menu._oId = optionWId
            , Menu._oRender =
                ( Widget.makeFocusableView <*> Widget.makeSubId optionId
                    <&> fmap
                ) <*> NameEdit.makeView (name ^. Name.form) optionId
                <&>
                \widget ->
                Menu.RenderedOption
                { Menu._rWidget = widget
                , Menu._rPick = Widget.PreEvent
                    { Widget._pDesc = "Pick"
                    , Widget._pAction =
                        (tag ^. Sugar.tagActions . Sugar.taChangeTag) t
                        <&> pickResult
                    , Widget._pTextRemainder = ""
                    }
                }
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
            }
            where
                pickResult tagInfo =
                    Menu.PickResult
                    { Menu._pickDest =
                        tagInfo ^. Sugar.tagInstance & WidgetIds.fromEntityId
                    , Menu._pickDestIsEntryPoint = False
                    }
                optionWId = WidgetIds.hash t
                optionId = Widget.toAnimId optionWId

allowedSearchTerm :: Text -> Bool
allowedSearchTerm = Text.all Char.isAlphaNum

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
        searchTerm <- GuiState.readWidgetState holeId <&> fromMaybe ""
        setNameEventMap <-
            tagId tag <$ setTagName tag searchTerm
            & makePickEventMap nearestHoles (E.Doc ["Edit", "Tag", "Set name"])
        term <-
            TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? searchTermId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ E.filter (allowedSearchTerm . fst)
            <&> Align.tValue . Lens.mapped %~ pure . updateState
            <&> Align.tValue %~ Widget.weakerEvents setNameEventMap
        tooltip <- Lens.view theme <&> Theme.tooltip
        topLine <-
            if not (Text.null searchTerm) && Widget.isFocused (term ^. Align.tValue)
            then do
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
        makeMenu <-
            makeOptions tag searchTerm
            >>= Menu.makeHovered Element.empty
                (nearestHoles ^. NearestHoles.next <&> WidgetIds.fromEntityId)
        topLine <&> makeMenu Menu.AnyPlace & pure
    & GuiState.assignCursor holeId searchTermId
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId holeId)
    where
        holeId = WidgetIds.tagHoleId (tagId tag)
        searchTermId = holeId <> Widget.Id ["term"]
        textEditNoEmpty = TextEdit.EmptyStrings "" ""
        updateState (newSearchTerm, update) =
            update <> GuiState.updateWidgetState holeId newSearchTerm

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
                (tag ^. Sugar.tagActions . Sugar.taReplaceWithNew
                    <&> (^. Sugar.tagInstance)
                    <&> WidgetIds.fromEntityId
                    <&> WidgetIds.tagHoleId)
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            NameEdit.makeView (tag ^. Sugar.tagName . Name.form) (Widget.toAnimId myId)
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

makeRecordTag ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasTheme env
    , HasConfig env, GuiState.HasState env
    , Element.HasAnimIdPrefix env, HasStdSpacing env
    , TextEdit.HasStyle env, Hover.HasStyle env, HasTheme env, Menu.HasConfig env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeRecordTag nearestHoles tag =
    do
        color <- Lens.view Theme.theme <&> Theme.name <&> Theme.recordTagColor
        makeTagEdit nearestHoles tag
            & Reader.local (TextView.color .~ color)

makeCaseTag ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasTheme env
    , HasConfig env, GuiState.HasState env
    , TextEdit.HasStyle env, Hover.HasStyle env, HasTheme env, Menu.HasConfig env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeCaseTag nearestHoles tag =
    do
        color <- Lens.view Theme.theme <&> Theme.name <&> Theme.caseTagColor
        makeTagEdit nearestHoles tag
            & Reader.local (TextView.color .~ color)

makeParamTag ::
    ( MonadReader env f, HasTheme env, HasConfig env, Hover.HasStyle env, Menu.HasConfig env
    , GuiState.HasState env, Element.HasAnimIdPrefix env
    , TextEdit.HasStyle env, HasStdSpacing env, MonadTransaction m f
    ) =>
    Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeParamTag tag =
    do
        color <- Lens.view Theme.theme <&> Theme.name <&> Theme.parameterColor
        makeTagEdit NearestHoles.none tag
            & Reader.local (TextView.color .~ color)

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag ::
    ( MonadReader env f, HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) => Name (T m) -> Sugar.EntityId -> f (WithTextPos View)
makeArgTag name entityId =
    do
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        NameEdit.makeView (name ^. Name.form) animId
            & Reader.local (TextView.color .~ Theme.paramTagColor nameTheme)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId
