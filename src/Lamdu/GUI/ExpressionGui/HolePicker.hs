{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionGui.HolePicker
    ( HolePicker(..), withHolePicker, setResultPicker
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as Writer
import qualified Data.Char as Char
import           Data.Store.Transaction (Transaction)
import qualified Data.Text.Lens as TextLens
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState

import           Lamdu.Prelude

type T = Transaction

data HolePicker m
    = NoHolePick
    | HolePick (T m GuiState.Update)

instance Monoid (HolePicker m) where
    mempty = NoHolePick
    mappend NoHolePick x = x
    mappend x NoHolePick = x
    mappend _ _ = error "Two HolePick's told, are we inside 2 holes simultaneously?"

withHolePicker :: Monad m => HolePicker m -> E.EventMap (T m a) -> E.EventMap (T m a)
withHolePicker NoHolePick e = e
withHolePicker (HolePick action) e =
    e
    & E.emDocs . E.docStrs . Lens.reversed . Lens.element 0 %~ f
    <&> (action >>)
    where
        f x =
            x
            & TextLens._Text . Lens.element 0 %~ Char.toLower
            & ("Pick result and " <>)

setResultPicker :: MonadWriter (HolePicker n) m => T n GuiState.Update -> m ()
setResultPicker = Writer.tell . HolePick
