-- | Test your GUI with artifically supplied input events
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GUI.Momentu.Test
    ( TestEvent(..), teLookupEvent, teValidateEvent, teValidateNewGUIState
    , Event(..)
    , mainLoop
    ) where

-- import qualified GUI.Momentu.ModKey as ModKey
import qualified Control.Lens as Lens
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.Main as Main
import           GUI.Momentu.State (GUIState)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget, R)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Main.Events (Event(..))
import           Data.Vector.Vector2 (Vector2(..))
import           Control.Monad (foldM)

import           GUI.Momentu.Prelude hiding (lookup)

data TestEvent m a = TestEvent
    { _teLookupEvent :: EventMap a -> Maybe a
    , _teValidateEvent :: Maybe a -> m ()
    , _teValidateNewGUIState :: GUIState -> m ()
    }
Lens.makeLenses ''TestEvent

surrounding :: Vector2 R -> Vector2 R -> Widget.Surrounding
surrounding winSize widgetSize =
    Widget.Surrounding
    { Widget._sLeft = 0
    , Widget._sTop = 0
    , Widget._sRight = right
    , Widget._sBottom = bottom
    }
    where
        Vector2 right bottom = winSize - widgetSize

mkEventCtx :: Widget.Focused a -> Maybe State.VirtualCursor -> Widget.EventContext
mkEventCtx focused virtCursor =
    Widget.EventContext
    { Widget._eVirtualCursor =
        fromMaybe (State.VirtualCursor focalArea) virtCursor
    , Widget._ePrevTextRemainder = ""
    }
    where
        focalArea =
            focused ^? Widget.fFocalAreas . Lens.reversed . Lens.ix 0
            & fromMaybe (error "No focal areas in focused widget!")

applyUpdate ::
    Monad m =>
    m State.Update -> Main.Env -> m (Main.Env, Maybe State.VirtualCursor)
applyUpdate act env =
    act
    <&>
    \update ->
    ( State.update update env
    , update ^. State.uVirtualCursor . Lens._Wrapped
    )

mainLoop ::
    forall m.
    MonadFail m =>
    Main.Env -> (Main.Env -> m (Widget m)) ->
    [TestEvent m (m State.Update)] -> m ()
mainLoop initEnv mkWidget =
    foldM step (initEnv, Nothing)
    <&> void
    where
        step (env, virtCursor) (TestEvent lookup validateEvent validateState) =
            do
                widget <- mkWidget env
                mkFocused <-
                    widget ^? Widget.wState . Widget._StateFocused
                    & maybe (fail "Unfocused widget generated in test") pure
                let focused =
                        surrounding (env ^. Main.eWindowSize) (widget ^. Widget.wSize)
                        & mkFocused
                let mkEventMap = focused ^. Widget.fEventMap
                let eventMap = mkEventMap (mkEventCtx focused virtCursor)
                let res = lookup eventMap
                validateEvent res
                (newEnv, newVirtCursor) <-
                    case res of
                    Nothing -> pure (env, virtCursor)
                    Just act -> applyUpdate act env
                validateState (newEnv ^. Main.eState)
                pure (newEnv, newVirtCursor)
