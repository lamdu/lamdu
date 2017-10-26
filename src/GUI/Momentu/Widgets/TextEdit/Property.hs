-- | TextEdit creation functions that are based on Property instead of
-- events yielding new texts
{-# LANGUAGE NoImplicitPrelude #-}
module GUI.Momentu.Widgets.TextEdit.Property
    ( make, makeLineEdit, makeWordEdit
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

import           Lamdu.Prelude

make ::
    (MonadReader env m, Applicative f,
     State.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     WithTextPos (Widget (f State.Update)))
make =
    TextEdit.make <&> f
    where
        f mk empty textRef myId =
            mk empty (Property.value textRef) myId
            & Align.tValue . Lens.mapped %~ setter
            where
                setter (newText, eventRes) =
                    eventRes <$
                    when (newText /= Property.value textRef) (Property.set textRef newText)

deleteKeyEventHandler :: E.HasEventMap f => ModKey -> f a -> f a
deleteKeyEventHandler key =
    E.eventMap %~ E.deleteKey (E.KeyEvent ModKey.KeyState'Pressed key)

makeLineEdit ::
    (MonadReader env m, Applicative f, State.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     WithTextPos (Widget (f State.Update)))
makeLineEdit =
    make
    <&> \mk empty textRef myId ->
    mk empty textRef myId
    & Align.tValue %~ deleteKeyEventHandler (ModKey mempty ModKey.Key'Enter)

makeWordEdit ::
    (MonadReader env m, Applicative f, State.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     WithTextPos (Widget (f State.Update)))
makeWordEdit =
    makeLineEdit
    <&> \mk empty textRef myId -> mk empty textRef myId
    & Align.tValue %~ deleteKeyEventHandler (ModKey mempty ModKey.Key'Space)
