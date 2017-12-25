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
    , HasSearchStringRemainder(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as Writer
import qualified Data.Char as Char
import qualified Data.Text.Lens as TextLens
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E

import           Lamdu.Prelude

class HasSearchStringRemainder env where searchStringRemainder :: Lens' env Text

class HasPickers m where
    type PickerM m :: * -> *
    listenPicker :: m a -> m (a, Picker (PickerM m))

data Info m = Info
    { iAction :: m ()
    , iSearchStringRemainder :: Text
    }

data Picker m
    = NoPick
    | Pick (Info m)

instance Monoid (Picker m) where
    mempty = NoPick
    mappend NoPick x = x
    mappend x NoPick = x
    mappend _ _ = error "Two Pick's told, are we inside 2 holes simultaneously?"

withPicker ::
    (MonadReader env m, HasSearchStringRemainder env, Monad f) =>
    Picker f -> (Text -> EventMap (f a)) -> m (EventMap (f a))
withPicker NoPick mk = Lens.view searchStringRemainder <&> mk
withPicker (Pick h) mk =
    mk (iSearchStringRemainder h)
    & E.emDocs . E.docStrs . Lens.reversed . Lens.element 0 %~ f
    <&> (iAction h >>)
    & pure
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
