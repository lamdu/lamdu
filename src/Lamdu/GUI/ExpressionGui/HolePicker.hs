{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionGui.HolePicker
    ( HolePicker(..), withHolePicker, tellResultPicker
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

-- When search string is "42.", and picking the "42" result,
-- the "." is the search string remainder.
class HasSearchStringRemainder env where searchStringRemainder :: Lens' env Text

data Info m = Info
    { iAction :: m ()
    , iSearchStringRemainder :: Text
    }

data HolePicker m
    = NoHolePick
    | HolePick (Info m)

instance Monoid (HolePicker m) where
    mempty = NoHolePick
    mappend NoHolePick x = x
    mappend x NoHolePick = x
    mappend _ _ = error "Two HolePick's told, are we inside 2 holes simultaneously?"

withHolePicker ::
    (MonadReader env m, HasSearchStringRemainder env, Monad f) =>
    HolePicker f -> (Text -> EventMap (f a)) -> m (EventMap (f a))
withHolePicker NoHolePick mk = Lens.view searchStringRemainder <&> mk
withHolePicker (HolePick h) mk =
    mk (iSearchStringRemainder h)
    & E.emDocs . E.docStrs . Lens.reversed . Lens.element 0 %~ f
    <&> (iAction h >>)
    & pure
    where
        f x =
            x
            & TextLens._Text . Lens.element 0 %~ Char.toLower
            & ("Pick result and " <>)

tellResultPicker :: MonadWriter (HolePicker n) m => Text -> n () -> m ()
tellResultPicker remainder act =
    HolePick Info
    { iAction = act
    , iSearchStringRemainder = remainder
    }
    & Writer.tell
