{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator
    ( FocusEntryTarget(..)
    , Config(..)
    , make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (MonadReader)
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey, toModKey)
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

import           Lamdu.Prelude

data FocusEntryTarget = FocusEntryChild | FocusEntryParent

data Config = Config
    { focusChildKeys :: [MetaKey]
    , focusChildDoc :: E.Doc
    , focusParentKeys :: [MetaKey]
    , focusParentDoc :: E.Doc
    }

setFocusChildEventMap :: Config -> Widget a -> Widget a
setFocusChildEventMap Config{..} widgetRecord =
    widgetRecord
    -- We're not delegating, so replace the child eventmap with an
    -- event map to either delegate to it (if it is enterable) or to
    -- nothing (if it is not):
    & Widget.eventMap .~ neeventMap
    where
        neeventMap =
            case widgetRecord ^. Widget.mEnter of
            Nothing -> mempty
            Just childEnter ->
                E.keyPresses (focusChildKeys <&> toModKey) focusChildDoc $
                childEnter Direction.Outside ^. Widget.enterResultEvent

modifyEntry ::
    Applicative f =>
    Widget.Id -> Rect -> FocusEntryTarget ->
    Widget.MEnter (f Widget.EventResult) ->
    Widget.MEnter (f Widget.EventResult)
modifyEntry myId fullChildRect = f
    where
        f FocusEntryParent _ = Just $ const focusParent
        f FocusEntryChild Nothing = Just $ const focusParent
        f FocusEntryChild (Just childEnter) = Just $ wrapEnter childEnter
        wrapEnter _          Direction.Outside = focusParent
        wrapEnter enterChild dir               = enterChild dir

        focusParent =
            Widget.EnterResult
            { Widget._enterResultRect = fullChildRect
            , Widget._enterResultEvent = pure $ Widget.eventResultFromCursor myId
            }

make ::
    (MonadReader env m, Widget.HasCursor env, Applicative f) =>
    Config -> FocusEntryTarget -> Widget.Id ->
    m (Widget (f Widget.EventResult) -> Widget (f Widget.EventResult))
make config@Config{..} focusEntryTarget myId =
    do
        cursor <- Lens.view Widget.cursor
        return $ \childWidget ->
            case () of
            ()
                | selfIsFocused ->
                    Widget.respondToCursor childWidget
                    & setFocusChildEventMap config
                    -- NOTE: Intentionally not checking whether child is also
                    -- focused. That's a bug, which will usefully show up as two
                    -- cursors displaying rather than a crash.

                | childIsFocused ->
                    childWidget
                    & Widget.weakerEvents focusParentEventMap

                | otherwise ->
                    childWidget
                    & Widget.mEnter %~ modifyEntry myId fullChildRect focusEntryTarget
                where
                    fullChildRect = Rect 0 (childWidget ^. Widget.size)
                    childIsFocused = Widget.isFocused childWidget
                    selfIsFocused = myId == cursor
                    focusParentEventMap =
                        Widget.keysEventMapMovesCursor focusParentKeys focusParentDoc
                        (pure myId)
