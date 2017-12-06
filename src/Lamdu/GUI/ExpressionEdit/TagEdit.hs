{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.TagEdit
    ( makeRecordTag, makeCaseTag, Mode(..)
    , makeParamTag
    , makeArgTag
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Function (on)
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
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
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing)
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.CharClassification as Chars
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
        config <- Lens.view Config.config <&> Config.hole
        let keys = Config.holePickAndMoveToNextHoleKeys config
        let jumpNextEventMap =
                nearestHoles ^. NearestHoles.next
                & maybe mempty
                  (Widget.keysEventMapMovesCursor keys
                   (E.Doc ["Navigation", "Jump to next hole"]) .
                   return . WidgetIds.fromEntityId)
        NameEdit.makeBareEdit
            (tag ^. Sugar.tagName & Name.setName .~ setTagName tag)
            (tagRenameId myId)
            <&> Align.tValue . E.eventMap %~ E.filterChars (/= ',')
            <&> Align.tValue %~ E.weakerEvents jumpNextEventMap
            <&> Align.tValue %~ E.weakerEvents stopEditingEventMap
    where
        myId = WidgetIds.fromEntityId (tag ^. Sugar.tagInfo . Sugar.tagInstance)
        stopEditingEventMap =
            Widget.keysEventMapMovesCursor
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
    m (Widget.EventMap (f GuiState.Update))
makePickEventMap nearestHoles doc action =
    Lens.view Config.config <&> Config.hole <&>
    \config ->
    let pickKeys = Config.holePickResultKeys config
        jumpNextKeys = Config.holePickAndMoveToNextHoleKeys config
    in
    case nearestHoles ^. NearestHoles.next of
    Nothing -> Widget.keysEventMapMovesCursor (pickKeys <> jumpNextKeys) doc action
    Just nextHole ->
        Widget.keysEventMapMovesCursor pickKeys doc action
        <> Widget.keysEventMapMovesCursor jumpNextKeys
            (doc & E.docStrs . Lens.reversed . Lens.ix 0 %~ (<> " and jump to next hole"))
            (WidgetIds.fromEntityId nextHole <$ action)

makeOptions ::
    ( Monad m, MonadTransaction m f, MonadReader env f, GuiState.HasCursor env
    , HasConfig env, HasTheme env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    NearestHoles -> Sugar.Tag (Name n) (T m) -> Text ->
    f ([Menu.Option f (T m GuiState.Update)], Menu.HasMoreOptions)
makeOptions nearestHoles tag searchTerm
    | Text.null searchTerm = pure ([], Menu.NoMoreOptions)
    | otherwise =
        do
            resultCount <-
                Lens.view Config.config
                <&> Config.hole <&> Config.holeResultCount
            tag ^. Sugar.tagActions . Sugar.taOptions & transaction
                <&> filter (Lens.anyOf (_1 . Name.form . Name._Stored . _1) (insensitiveInfixOf searchTerm))
                <&> splitAt resultCount
                <&> _2 %~ checkHasMoreResult
                >>= _1 . traverse %%~ makeOption
    where
        checkHasMoreResult [] = Menu.NoMoreOptions
        checkHasMoreResult (_:_) = Menu.MoreOptionsAvailable
        makeOption (name, t) =
            do
                eventMap <-
                    (tag ^. Sugar.tagActions . Sugar.taChangeTag) t
                    <&> (^. Sugar.tagInstance)
                    <&> WidgetIds.fromEntityId
                    & makePickEventMap nearestHoles (E.Doc ["Edit", "Tag", "Select"])
                (Widget.makeFocusableView <*> Widget.makeSubId optionId <&> fmap)
                    <*> NameEdit.makeView (name ^. Name.form) optionId
                    <&> Align.tValue %~ E.weakerEvents eventMap
            <&> Menu.Option optionWId ?? Menu.SubmenuEmpty
            where
                optionWId = WidgetIds.hash t
                optionId = Widget.toAnimId optionWId

makeTagHoleEdit ::
    ( Monad m, MonadReader env f, MonadTransaction m f
    , GuiState.HasCursor env, GuiState.HasWidgetState env
    , HasConfig env, TextEdit.HasStyle env, Element.HasAnimIdPrefix env
    , HasTheme env, Hover.HasStyle env, Menu.HasStyle env, HasStdSpacing env
    ) => NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeTagHoleEdit nearestHoles tag =
    do
        searchTerm <- GuiState.readWidgetState holeId <&> fromMaybe ""
        setNameEventMap <-
            tagId tag <$ setTagName tag searchTerm
            & makePickEventMap nearestHoles (E.Doc ["Edit", "Tag", "Set name"])
        term <-
            TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? searchTermId
            <&> Align.tValue . Lens.mapped %~ pure . updateState
            <&> Align.tValue %~ E.strongerEvents setNameEventMap
            <&> Align.tValue %~ E.filterChars (`notElem` Chars.disallowedInHole)
        tooltip <- Lens.view theme <&> Theme.tooltip
        topLine <-
            if not (Text.null searchTerm) && Widget.isFocused (term ^. Align.tValue)
            then do
                label <- (TextView.make ?? "(new tag)") <*> Element.subAnimId ["label"]
                space <- Spacer.stdHSpace
                hover <- Hover.hover
                let anchor = fmap Hover.anchor
                let hLabel = hover label & Hover.sequenceHover
                let hoverOptions =
                        [ anchor (term /|/ space) /|/ hLabel
                        , hLabel /|/ anchor (space /|/ term)
                        ] <&> (^. Align.tValue)
                anchor term
                    <&> Hover.hoverInPlaceOf hoverOptions
                    & pure
                & Reader.local (Hover.backgroundColor .~ Theme.tooltipBgColor tooltip)
                & Reader.local (TextView.color .~ Theme.tooltipFgColor tooltip)
                & Reader.local (Element.animIdPrefix <>~ ["label"])
            else pure term
        (options, hasMore) <- makeOptions nearestHoles tag searchTerm
        hoverMenu <- Menu.hoverMenu
        menu <- Menu.layout 0 options hasMore
        topLine <&> hoverMenu ?? menu & pure
    & GuiState.assignCursor holeId searchTermId
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId holeId)
    where
        holeId = WidgetIds.tagHoleId (tagId tag)
        searchTermId = holeId <> Widget.Id ["term"]
        textEditNoEmpty = TextEdit.EmptyStrings "" ""
        updateState (newSearchTerm, update) =
            update <> GuiState.updateWidgetState holeId newSearchTerm

data Mode = WithTagHoles | WithoutTagHoles

makeTagEdit ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasConfig env
    , GuiState.HasCursor env, GuiState.HasWidgetState env
    , HasTheme env, Element.HasAnimIdPrefix env
    , TextEdit.HasStyle env, Hover.HasStyle env, Menu.HasStyle env
    , HasStdSpacing env
    ) =>
    Mode -> Draw.Color -> NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeTagEdit mode tagColor nearestHoles tag =
    do
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        isRenaming <- GuiState.isSubCursor ?? tagRenameId myId
        isHole <- GuiState.isSubCursor ?? WidgetIds.tagHoleId myId
        config <- Lens.view Config.config
        let eventMap =
                Widget.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
                (E.Doc ["Edit", "Tag", "Open"]) (pure (tagRenameId myId))
                <>
                case mode of
                WithTagHoles ->
                    Widget.keysEventMapMovesCursor (Config.delKeys config)
                    (E.Doc ["Edit", "Tag", "Choose"])
                    (tag ^. Sugar.tagActions . Sugar.taReplaceWithNew
                        <&> (^. Sugar.tagInstance)
                        <&> WidgetIds.fromEntityId
                        <&> WidgetIds.tagHoleId)
                WithoutTagHoles -> mempty
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            NameEdit.makeView (tag ^. Sugar.tagName . Name.form) (Widget.toAnimId myId)
            <&> Lens.mapped %~ E.weakerEvents eventMap
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
            <&> E.weakerEvents jumpHolesEventMap
            & pure
    & Reader.local (TextView.color .~ tagColor)
    & GuiState.assignCursor myId viewId
    where
        myId = WidgetIds.fromEntityId (tag ^. Sugar.tagInfo . Sugar.tagInstance)
        viewId = tagViewId myId

makeRecordTag ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasTheme env
    , HasConfig env, GuiState.HasCursor env, GuiState.HasWidgetState env
    , Element.HasAnimIdPrefix env, HasStdSpacing env
    , TextEdit.HasStyle env, Hover.HasStyle env, HasTheme env, Menu.HasStyle env
    ) =>
    Mode -> NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeRecordTag mode nearestHoles tag =
    do
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        makeTagEdit mode (Theme.recordTagColor nameTheme) nearestHoles tag

makeCaseTag ::
    ( Monad m, MonadReader env f, MonadTransaction m f, HasTheme env
    , HasConfig env, GuiState.HasCursor env, GuiState.HasWidgetState env
    , TextEdit.HasStyle env, Hover.HasStyle env, HasTheme env, Menu.HasStyle env
    , HasStdSpacing env, Element.HasAnimIdPrefix env
    ) =>
    NearestHoles -> Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeCaseTag nearestHoles tag =
    do
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        makeTagEdit WithTagHoles (Theme.caseTagColor nameTheme) nearestHoles tag

makeParamTag ::
    ( MonadReader env f, HasTheme env, HasConfig env, Hover.HasStyle env, Menu.HasStyle env
    , GuiState.HasCursor env, GuiState.HasWidgetState env, Element.HasAnimIdPrefix env
    , TextEdit.HasStyle env, HasStdSpacing env, MonadTransaction m f
    ) =>
    Sugar.Tag (Name (T m)) (T m) ->
    f (WithTextPos (Widget (T m GuiState.Update)))
makeParamTag tag =
    do
        paramColor <- Lens.view Theme.theme <&> Theme.name <&> Theme.parameterColor
        makeTagEdit WithTagHoles paramColor NearestHoles.none tag

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
