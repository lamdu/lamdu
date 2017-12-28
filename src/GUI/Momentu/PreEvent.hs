-- | For effective text-like keyboard based input, current choices
-- should be finalized without an explicit action, upon further
-- action.  For example when creating a list, and choosing an item by
-- moving the cursor to it, pressing "," or "+" to add another item
-- should finalize the selection of the currently focused item.  This
-- selection of current item is the PreEvent to the other event.

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module GUI.Momentu.PreEvent
    ( PreEvents, PreEvent(..), withPreEvents, tellPreEvent
    , HasPreEvents(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Writer (MonadWriter, tell)
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E

import           Lamdu.Prelude

class HasPreEvents m where
    type Event m
    listenPreEvents :: m a -> m (a, [PreEvent (Event m)])

data PreEvent a = PreEvent
    { pDesc :: E.Subtitle
    , pAction :: a
    , pTextRemainder :: Text
    }

type PreEvents a = [PreEvent a]

actionText :: Lens.Traversal' (EventMap a) E.Subtitle
actionText = E.emDocs . E.docStrs . Lens.reversed . Lens.element 0

withPreEvents :: Monad f => [PreEvent (f ())] -> (Text, EventMap (f a) -> EventMap (f a))
withPreEvents pres =
    ( mconcat (pres <&> pTextRemainder)
    , onEventMap
    )
    where
        onEventMap eventMap =
            eventMap
            & actionText %~ onActionText
            <&> (mapM_ pAction pres >>)
        onActionText x = (pres <&> pDesc) ++ [x] & filter (not . Text.null) & Text.intercalate ", "

tellPreEvent :: MonadWriter (PreEvents a) m => PreEvent a -> m ()
tellPreEvent = tell . (:[])
