{-# LANGUAGE FlexibleContexts #-}
-- | Event-loop for GLFW (instead of its native callbacks model).

module Graphics.UI.GLFW.Events
    ( eventLoop
    , Event(..), KeyEvent(..), MouseButtonEvent(..)
    , Next(..)
    ) where

import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (when)
import           Data.IORef.Extended
import           Data.Typeable (Typeable)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW

import           Prelude

-- GLFWRawEvent is the reification of the callback information.
-- It differs from Event in that in some cases events are grouped together
-- (i.e a RawKeyEvent and a RawCharEvent become a combined KeyEvent).
data GLFWRawEvent
    = RawCharEvent Char
    | RawKeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
    | RawMouseButton GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
    | RawWindowRefresh
    | RawDropPaths [FilePath]
    | RawFrameBufferSize (Vector2 Int)
    | RawWindowClose
    deriving (Show, Eq)

data KeyEvent = KeyEvent
    { keKey :: GLFW.Key
    , keScanCode :: Int
    , keState :: GLFW.KeyState
    , keModKeys :: GLFW.ModifierKeys
    } deriving (Show, Eq)

data MouseButtonEvent = MouseButtonEvent
    { mbButton :: GLFW.MouseButton
    , mbButtonState :: GLFW.MouseButtonState
    , mbModKeys :: GLFW.ModifierKeys
    , mbPosition :: Vector2 Double
    -- ^ Position in frame buffer coordinates, which may not be the same as
    -- "window coordinates"
    , mbPositionInWindowCoords :: Vector2 Double
    -- ^ Position in "window coordinates".
    -- Since "retina" displays were introduced, window coordindates no longer
    -- relate to graphical pixels.
    } deriving (Show, Eq)

data Event
    = EventKey KeyEvent
    | EventChar Char
    | EventMouseButton MouseButtonEvent
    | EventWindowClose
    | EventWindowRefresh
    | EventDropPaths [FilePath]
    | EventFrameBufferSize (Vector2 Int)
    deriving (Show, Eq)

-- | The output of the event handler back to the event-loop.
data Next
    = NextWait
    -- ^ idle wait for the next event
    | NextPoll
    -- ^ poll for the next event and immediately (up to sync-to-vblank) continue
    | NextQuit
    -- ^ The event-loop should quit.
    deriving (Show, Eq, Ord)

fromChar :: Char -> Maybe Char
fromChar char
    -- Range for "key" characters (keys for left key, right key, etc.)
    | '\57344' <= char && char <= '\63743' = Nothing
    | otherwise = Just char

translate ::
    GLFW.Window -> [GLFWRawEvent] -> IO [Event]
translate _ [] = pure []
translate win (x : xs) =
    case x of
    RawWindowClose -> simple EventWindowClose
    RawWindowRefresh -> simple EventWindowRefresh
    RawDropPaths paths -> simple (EventDropPaths paths)
    RawCharEvent r ->
        case fromChar r of
        Just c -> simple (EventChar c)
        Nothing -> translate win xs
    RawKeyEvent key scanCode keyState modKeys ->
        KeyEvent key scanCode keyState modKeys & EventKey & simple
    RawFrameBufferSize size -> simple (EventFrameBufferSize size)
    RawMouseButton button buttonState modKeys ->
        do
            fbSize <- GLFW.getFramebufferSize win <&> uncurry Vector2 <&> fmap fromIntegral
            winSize <- GLFW.getWindowSize win <&> uncurry Vector2 <&> fmap fromIntegral
            p <- GLFW.getCursorPos win <&> uncurry Vector2
            simple $ EventMouseButton $
                MouseButtonEvent
                { mbButton = button
                , mbButtonState = buttonState
                , mbModKeys = modKeys
                , mbPosition = p * fbSize / winSize
                , mbPositionInWindowCoords = p
                }
    where
        simple out = translate win xs <&> (out :)

data EventLoopDisallowedWhenMasked = EventLoopDisallowedWhenMasked
    deriving (Show, Typeable)
instance E.Exception EventLoopDisallowedWhenMasked

eventLoop :: GLFW.Window -> ([Event] -> IO Next) -> IO ()
eventLoop win eventsHandler =
    do
        maskingState <- E.getMaskingState
        when (maskingState /= E.Unmasked) $ E.throwIO EventLoopDisallowedWhenMasked
        eventsVar <- newIORef [RawWindowRefresh]
        let addEvent event = atomicModifyIORef_ eventsVar (event:)
            addKeyEvent key scanCode keyState modKeys =
                addEvent $ RawKeyEvent key scanCode keyState modKeys
            addMouseButtonEvent button buttonState modKeys =
                addEvent $ RawMouseButton button buttonState modKeys
            setCallback f cb = f win $ Just $ const cb
            addVec2Event c x y = addEvent $ c $ Vector2 x y
            loop =
                do
                    let handleReversedEvents rEvents = ([], reverse rEvents)
                    rawEvents <-
                        atomicModifyIORef eventsVar handleReversedEvents
                    events <- translate win rawEvents
                    next <- eventsHandler events
                    case next of
                        NextWait ->
                            do
                                GLFW.waitEvents
                                loop
                        NextPoll ->
                            do
                                GLFW.pollEvents
                                loop
                        NextQuit -> pure ()

        setCallback GLFW.setCharCallback (addEvent . RawCharEvent)
        setCallback GLFW.setKeyCallback addKeyEvent
        setCallback GLFW.setMouseButtonCallback addMouseButtonEvent
        setCallback GLFW.setDropCallback (addEvent . RawDropPaths)
        setCallback GLFW.setWindowRefreshCallback $ addEvent RawWindowRefresh
        setCallback GLFW.setFramebufferSizeCallback $ addVec2Event RawFrameBufferSize
        setCallback GLFW.setWindowCloseCallback $ addEvent RawWindowClose

        GLFW.swapInterval 1
        loop
