-- | When shortcut keys are pressed to perform results,
-- Such as adding an element to a list, they often should implicitly
-- perform the selection of the currently chosen menu item.
--
-- When the menu is a search-menu, they may also work differently
-- depending on ambigiuous remainders of a search term which
-- become unambigious given the new key pressed.

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts, TypeFamilies #-}

module GUI.Momentu.Widgets.Menu.Picker
    ( Picker(..), withPicker, tellPicker
    , HasPickers(..)
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

class HasPickers m where
    type PickerM m :: * -> *
    listenPicker :: m a -> m (a, Picker (PickerM m))

data Info m = Info
    { iAction :: m ()
    , iSearchStringRemainder :: Text
    }

data Picker m
    = -- Search string remainder,
      -- For when in a result
      NoPick Text
    | Pick (Info m)

instance Monoid (Picker m) where
    mempty = NoPick ""
    mappend (NoPick x) (NoPick y)
        | Text.null x = NoPick y
        | Text.null y = NoPick x
        | otherwise = error "Two picks!"
    mappend (NoPick x) (Pick y)
        | Text.null x = Pick y
        | otherwise = error "Two Picks"
    mappend (Pick x) (NoPick y)
        | Text.null y = Pick x
        | otherwise = error "Two Picks"
    mappend Pick{} Pick{} = error "Two Pick's told, are we inside 2 holes simultaneously?"

actionText :: Lens.Traversal' (EventMap a) E.Subtitle
actionText = E.emDocs . E.docStrs . Lens.reversed . Lens.element 0

withPicker :: Monad f => Picker f -> (Text -> EventMap (f a)) -> EventMap (f a)
withPicker (NoPick x) mk = mk x
withPicker (Pick h) mk =
    mk (iSearchStringRemainder h)
    & actionText %~ f
    <&> (iAction h >>)
    where
        f x =
            x
            & TextLens._Text . Lens.element 0 %~ Char.toLower
            & ("Pick result and " <>)

tellPicker :: MonadWriter (Picker n) m => Text -> n () -> m ()
tellPicker remainder act =
    Pick Info
    { iAction = act
    , iSearchStringRemainder = remainder
    }
    & Writer.tell
