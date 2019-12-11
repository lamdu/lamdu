-- | TextEdit creation functions that are based on Property instead of
-- events yielding new texts
module GUI.Momentu.Widgets.TextEdit.Property
    ( make, makeLineEdit, makeWordEdit
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

import           GUI.Momentu.Prelude

make ::
    ( MonadReader env m, Applicative f
    , State.HasCursor env, TextEdit.Deps env
    ) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     TextWidget f)
make =
    TextEdit.make <&> f
    where
        f mk empty textRef myId =
            mk empty (Property.value textRef) myId
            & Align.tValue . Widget.updates %~ setter
            where
                setter (newText, eventRes) =
                    eventRes <$
                    when (newText /= Property.value textRef)
                    (newText & textRef ^. Property.pSet)

deleteKeyEventHandler :: ModKey -> EventMap a -> EventMap a
deleteKeyEventHandler = E.deleteKey . E.KeyEvent ModKey.KeyState'Pressed

makeLineEdit ::
    ( MonadReader env m, Applicative f
    , State.HasCursor env, TextEdit.Deps env
    ) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id ->
     TextWidget f)
makeLineEdit =
    make
    <&> \mk empty textRef myId ->
    mk empty textRef myId
    & Align.tValue . Widget.eventMapMaker . Lens.mapped %~
    deleteKeyEventHandler (ModKey mempty ModKey.Key'Enter)

makeWordEdit ::
    ( MonadReader env m, Applicative f
    , State.HasCursor env, TextEdit.Deps env
    ) =>
    m
    (TextEdit.EmptyStrings -> Property f Text -> Widget.Id -> TextWidget f)
makeWordEdit =
    makeLineEdit
    <&> \mk empty textRef myId -> mk empty textRef myId
    & Align.tValue . Widget.eventMapMaker . Lens.mapped %~
    deleteKeyEventHandler (ModKey mempty ModKey.Key'Space)
