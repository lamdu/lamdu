{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Widgets.Menu.Search
    ( emptyPickEventMap
    , resultsIdPrefix
    , ResultsContext(..), rSearchTerm, rResultIdPrefix
    , basicSearchTermEdit, searchTermEditEventMap, addPickFirstResultEvent
    , enterWithSearchTerm
    , make

    -- temporary exports that will be removed when transition of HoleEdit
    -- to Menu.Search is complete
    , readSearchTerm
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos, View)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.State (HasState(..))
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget.Id (Id(..), joinId)
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

-- | Context necessary for creation of menu items for a search.
data ResultsContext = ResultsContext
    { _rSearchTerm :: Text
    , _rResultIdPrefix :: Id
    } deriving (Eq, Show, Ord)
Lens.makeLenses ''ResultsContext

emptyPickEventMap ::
    (MonadReader env m, Menu.HasConfig env, Applicative f) =>
    m (EventMap (f State.Update))
emptyPickEventMap =
    Lens.view (Menu.config . Menu.configKeys)
    <&> allPickKeys
    <&> \keys -> E.keysEventMap keys (E.Doc ["Pick (N/A)"]) (pure ())
    where
        allPickKeys keys =
            keys ^. Menu.keysPickOption <>
            keys ^. Menu.keysPickOptionAndGotoNext

-- | All search menu results must start with a common prefix.
-- This is used to tell when cursor was on a result that got filtered out
-- when the search term changed in order to redirect it to a result.
resultsIdPrefix :: Id -> Id
resultsIdPrefix = (`joinId` ["Results"])

searchTermEditId :: Id -> Id
searchTermEditId = (`joinId` ["SearchTerm"])

readSearchTerm :: (MonadReader env m, HasState env) => Id -> m Text
readSearchTerm x = State.readWidgetState x <&> fromMaybe ""

basicSearchTermEdit ::
    (MonadReader env m, TextEdit.HasStyle env, HasState env, Applicative f) =>
    Id -> (Text -> Bool) -> m (WithTextPos (Widget (f State.Update)))
basicSearchTermEdit myId allowedSearchTerm =
    do
        searchTerm <- readSearchTerm myId
        let onEvents (newSearchTerm, eventRes)
                | newSearchTerm == searchTerm = eventRes
                | otherwise =
                    eventRes
                    <> State.updateWidgetState myId newSearchTerm
                    -- When first letter is typed in search term, jump to the
                    -- results, which will go to first result:
                    & ( if Text.null searchTerm
                        then
                            State.uCursor .~
                            (Just (resultsIdPrefix myId) ^. Lens._Unwrapped)
                        else id
                    )
        TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? searchTermEditId myId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                E.filter (allowedSearchTerm . fst)
            <&> Align.tValue . Lens.mapped %~ pure . onEvents
    where
        textEditNoEmpty = TextEdit.EmptyStrings "  " "  "

assignCursor ::
    (MonadReader env m, HasState env) =>
    Id -> [Id] -> m a -> m a
assignCursor myId resultIds action =
    do
        searchTerm <- readSearchTerm myId
        let destId
                | Text.null searchTerm =
                    -- When entering a hole with an empty search string
                    -- (Like after typing "factorial x="),
                    -- cursor should be on the search-string and not on a result
                    -- so that operators pressed will set the search string
                    -- rather than apply on the first result.
                    searchTermEditId myId
                | otherwise =
                      resultIds ^? Lens.traverse
                      & fromMaybe (Menu.noResultsId myId)

        -- Results appear and disappear when the search-string changes,
        -- but the cursor prefix signifies whether we should be on a result.
        -- When that is the case but is not currently on any of the existing results
        -- the cursor will be sent to the default one.
        shouldBeOnResult <- sub (resultsIdPrefix myId)
        isOnResult <- traverse sub resultIds <&> or

        action
            & if shouldBeOnResult && not isOnResult
            then Reader.local (State.cursor .~ destId)
            else State.assignCursor myId destId
    where
        sub x = State.isSubCursor ?? x

enterWithSearchTerm :: Text -> Id -> State.Update
enterWithSearchTerm searchTerm myId =
    State.updateCursor myId
    <> State.updateWidgetState myId searchTerm

make ::
    ( MonadReader env m, HasState env, Menu.HasConfig env
    , TextView.HasStyle env, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , Applicative f
    ) =>
    (Maybe (Widget.PreEvent (f Menu.PickResult)) ->
     m (WithTextPos (Widget (f State.Update)))) ->
    (ResultsContext -> m (Menu.OptionList (Menu.Option m f))) ->
    View -> Id ->
    m (Menu.Placement -> WithTextPos (Widget (f State.Update)))
make makeSearchTerm makeOptions annotation myId =
    readSearchTerm myId <&> (`ResultsContext` resultsIdPrefix myId)
    >>= makeOptions
    >>=
    \options ->
    do
        (mPickFirst, makeMenu) <- Menu.makeHovered myId annotation options
        makeSearchTerm mPickFirst
            <&> \searchTermWidget placement ->
                searchTermWidget <&> makeMenu placement
    & Reader.local (Element.animIdPrefix .~ toAnimId myId)
    & assignCursor myId (options ^.. traverse . Menu.oId)

-- Add events on search term to pick the first result.
addPickFirstResultEvent ::
    (MonadReader env m, Menu.HasConfig env, HasState env, Applicative f) =>
    Id -> Maybe (Widget.PreEvent (f Menu.PickResult))->
    m (Widget (f State.Update) -> Widget (f State.Update))
addPickFirstResultEvent myId mPickFirst =
    do
        pickEventMap <-
            case mPickFirst of
            Nothing -> emptyPickEventMap
            Just pickFirst -> Menu.makePickEventMap ?? pickFirst
        searchTerm <- readSearchTerm myId
        if Text.null searchTerm
            then pure (Widget.weakerEvents pickEventMap)
            else pure (addPre . Widget.weakerEvents pickEventMap)
    where
        addPre =
            Widget.wState . Widget._StateFocused . Lens.mapped .
            Widget.fPreEvents %~ (Widget.BlockEvents <>)

searchTermEditEventMap ::
    (MonadReader env m, HasState env) =>
    Id -> (Text -> Bool) -> m (EventMap State.Update)
searchTermEditEventMap myId allowedTerms =
    readSearchTerm myId
    <&>
    \searchTerm ->
    let appendCharEventMap =
            Text.snoc searchTerm
            & E.allChars "Character" (doc "Append character")
            -- We only filter when appending last char, not when
            -- deleting last char, because after appending deletion
            -- necessarily preserves any invariant we enforce in
            -- allowedTerms
            & E.filter allowedTerms
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                Text.init searchTerm
                & E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                    (doc "Delete backwards")
    in
    appendCharEventMap <> deleteCharEventMap
    <&> State.updateWidgetState myId
    where
        doc subtitle = E.Doc ["Edit", "Search Term", subtitle]
