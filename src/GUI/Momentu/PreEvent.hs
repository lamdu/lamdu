-- | For effective text-like keyboard based input,
-- current choices should be finalized without an explicit action,
-- upon further action.
-- For example when creating a list, and choosing an item by moving the cursor to it,
-- pressing "," or "+" to add another item should finalize the selection of the currently
-- focused item.
-- This selection of current item is the PreEvent to the other event.

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module GUI.Momentu.PreEvent
    ( PreEvents(..), withPreEvents, tellPreEvent
    , HasPreEvents(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as Writer
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Lens as TextLens
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E

import           Lamdu.Prelude

class HasPreEvents m where
    type EventM m :: * -> *
    listenPreEvents :: m a -> m (a, PreEvents (EventM m))

data Info m = Info
    { iAction :: m ()
    , iSearchStringRemainder :: Text
    }

data PreEvents m
    = -- Search string remainder,
      -- For when in a result
      PreText Text
    | Events (Info m)

instance Monoid (PreEvents m) where
    mempty = PreText ""
    mappend (PreText x) (PreText y)
        | Text.null x = PreText y
        | Text.null y = PreText x
        | otherwise = error "Two picks!"
    mappend (PreText x) (Events y)
        | Text.null x = Events y
        | otherwise = error "Two Picks"
    mappend (Events x) (PreText y)
        | Text.null y = Events x
        | otherwise = error "Two Picks"
    mappend Events{} Events{} = error "Two Pick's told, are we inside 2 holes simultaneously?"

actionText :: Lens.Traversal' (EventMap a) E.Subtitle
actionText = E.emDocs . E.docStrs . Lens.reversed . Lens.element 0

withPreEvents :: Monad f => PreEvents f -> (Text -> EventMap (f a)) -> EventMap (f a)
withPreEvents (PreText x) mk = mk x
withPreEvents (Events h) mk =
    mk (iSearchStringRemainder h)
    & actionText %~ f
    <&> (iAction h >>)
    where
        f x =
            x
            & TextLens._Text . Lens.element 0 %~ Char.toLower
            & ("Pick result and " <>)

tellPreEvent :: MonadWriter (PreEvents n) m => Text -> n () -> m ()
tellPreEvent remainder act =
    Events Info
    { iAction = act
    , iSearchStringRemainder = remainder
    }
    & Writer.tell
