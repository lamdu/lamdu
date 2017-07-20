-- | TextEdit creation functions that are based on Property instead of
-- events yielding new texts
{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Widgets.TextEdit.Property
    ( make, makeLineEdit, makeWordEdit
    ) where

import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

make ::
    (MonadReader env m, Applicative f,
     Widget.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     WithTextPos (Widget (f Widget.EventResult)))
make =
    TextEdit.make <&> f
    where
        f mk empty textRef myId =
            mk empty (Property.value textRef) myId
            & Align.tValue . Widget.events %~ setter
            where
                setter (newText, eventRes) =
                    eventRes <$
                    when (newText /= Property.value textRef) (Property.set textRef newText)

deleteKeyEventHandler :: E.HasEventMap f => ModKey -> f a -> f a
deleteKeyEventHandler key =
    E.eventMap %~ E.deleteKey (E.KeyEvent GLFW.KeyState'Pressed key)

makeLineEdit ::
    (MonadReader env m, Applicative f, Widget.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     WithTextPos (Widget (f Widget.EventResult)))
makeLineEdit =
    make
    <&> \mk empty textRef myId ->
    mk empty textRef myId
    & Align.tValue %~ deleteKeyEventHandler (ModKey mempty GLFW.Key'Enter)

makeWordEdit ::
    (MonadReader env m, Applicative f, Widget.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     WithTextPos (Widget (f Widget.EventResult)))
makeWordEdit =
    makeLineEdit
    <&> \mk empty textRef myId -> mk empty textRef myId
    & Align.tValue %~ deleteKeyEventHandler (ModKey mempty GLFW.Key'Space)
