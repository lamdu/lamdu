{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module GUI.Momentu.Widgets.Menu.Search
    ( emptyPickEventMap
    , resultsIdPrefix
    , ResultsContext(..), rSearchTerm, rResultIdPrefix

    -- temporary exports that will be removed when transition of HoleEdit
    -- to Menu.Search is complete
    , searchTermEditId
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget.Id (Id, joinId)
import qualified GUI.Momentu.Widgets.Menu as Menu

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
