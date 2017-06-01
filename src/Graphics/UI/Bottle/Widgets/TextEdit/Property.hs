-- | TextEdit creation functions that are based on Property instead of
-- events yielding new texts
{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Widgets.TextEdit.Property
    ( make, makeLineEdit, makeWordEdit
    ) where

import           Control.Monad.Reader (MonadReader)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import qualified Graphics.UI.Bottle.EventMap as EventMap
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
     Widget (f Widget.EventResult))
make =
    TextEdit.make <&> f
    where
        f mk empty textRef myId =
            mk empty (Property.value textRef) myId
            & Widget.events %~ setter
            where
                setter (newText, eventRes) =
                    eventRes <$
                    when (newText /= Property.value textRef) (Property.set textRef newText)

deleteKeyEventHandler :: ModKey -> Widget a -> Widget a
deleteKeyEventHandler key =
    Widget.eventMap %~
    EventMap.deleteKey (EventMap.KeyEvent GLFW.KeyState'Pressed key)

makeLineEdit ::
    (MonadReader env m, Applicative f, Widget.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     Widget (f Widget.EventResult))
makeLineEdit =
    make
    <&> \mk empty textRef myId ->
    mk empty textRef myId
    & deleteKeyEventHandler (ModKey mempty GLFW.Key'Enter)

makeWordEdit ::
    (MonadReader env m, Applicative f, Widget.HasCursor env, TextEdit.HasStyle env) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id -> Widget (f Widget.EventResult))
makeWordEdit =
    makeLineEdit
    <&> \mk empty textRef myId -> mk empty textRef myId
    & deleteKeyEventHandler (ModKey mempty GLFW.Key'Space)
