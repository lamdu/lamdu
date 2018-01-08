{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module GUI.Momentu.Widgets.Menu.Search
    ( emptyPickEventMap
    , resultsIdPrefix
    , ResultsContext(..), rSearchTerm, rResultIdPrefix
    , basicSearchTermEdit
    , enterWithSearchTerm

    -- temporary exports that will be removed when transition of HoleEdit
    -- to Menu.Search is complete
    , readSearchTerm, assignCursor
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as State
import           GUI.Momentu.State (HasState(..))
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget.Id (Id, joinId)
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

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
    Lens.view Menu.config
    <&> Menu.configKeys
    <&> (Menu.keysPickOption <> Menu.keysPickOptionAndGotoNext)
    <&> \keys -> E.keysEventMap keys (E.Doc ["Pick (N/A)"]) (pure ())

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
    (MonadReader env m, TextEdit.HasStyle env, HasState env) =>
    Id -> (Text -> Bool) -> m (WithTextPos (Widget State.Update))
basicSearchTermEdit searchMenuId allowedSearchTerm =
    do
        searchTerm <- readSearchTerm searchMenuId
        let onEvents (newSearchTerm, eventRes)
                | newSearchTerm == searchTerm = eventRes
                | otherwise =
                    eventRes
                    <> State.updateWidgetState searchMenuId newSearchTerm
                    -- When first letter is typed in search term, jump to the
                    -- results, which will go to first result:
                    & ( if Text.null searchTerm
                        then
                            State.uCursor .~
                            (Just (resultsIdPrefix searchMenuId) ^. Lens._Unwrapped)
                        else id
                    )
        TextEdit.make ?? textEditNoEmpty ?? searchTerm ?? searchTermEditId searchMenuId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                E.filter (allowedSearchTerm . fst)
            <&> Align.tValue . Lens.mapped %~ onEvents
    where
        textEditNoEmpty = TextEdit.EmptyStrings "  " "  "

assignCursor ::
    (MonadReader env m, HasState env) =>
    Widget.Id -> [Widget.Id] -> m a -> m a
assignCursor searchMenuId resultIds action =
    do
        searchTerm <- readSearchTerm searchMenuId
        let destId
                | Text.null searchTerm =
                    -- When entering a hole with an empty search string
                    -- (Like after typing "factorial x="),
                    -- cursor should be on the search-string and not on a result
                    -- so that operators pressed will set the search string
                    -- rather than apply on the first result.
                    searchTermEditId searchMenuId
                | otherwise = head (resultIds ++ [searchTermEditId searchMenuId])

        -- Results appear and disappear when the search-string changes,
        -- but the cursor prefix signifies whether we should be on a result.
        -- When that is the case but is not currently on any of the existing results
        -- the cursor will be sent to the default one.
        shouldBeOnResult <- sub (resultsIdPrefix searchMenuId)
        isOnResult <- traverse sub resultIds <&> or

        action
            & if shouldBeOnResult && not isOnResult
            then Reader.local (State.cursor .~ destId)
            else State.assignCursor searchMenuId destId
    where
        sub x = State.isSubCursor ?? x

enterWithSearchTerm :: Text -> Widget.Id -> State.Update
enterWithSearchTerm searchTerm searchMenuId =
    State.updateCursor searchMenuId
    <> State.updateWidgetState searchMenuId searchTerm
