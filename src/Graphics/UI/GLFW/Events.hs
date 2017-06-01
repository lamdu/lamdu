-- | Event-loop for GLFW (instead of its native callbacks model).

{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.GLFW.Events
    ( eventLoop
    , Event(..), KeyEvent(..), MouseButtonEvent(..)
    , Next(..)
    ) where

import qualified Control.Exception as E
import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import           Data.Typeable (Typeable)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

-- GLFWRawEvent is the reification of the callback information.
-- It differs from Event in that in some cases events are grouped together
-- (i.e a RawKeyEvent and a RawCharEvent become a combined KeyEvent).
data GLFWRawEvent
    = RawCharEvent Char
    | RawKeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
    | RawMouseButton GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
    | RawMousePosition (Vector2 Double)
    | RawWindowRefresh
    | RawDropPaths [FilePath]
    | RawFrameBufferSize (Vector2 Int)
    | RawWindowSize (Vector2 Int)
    | RawWindowClose
    deriving (Show, Eq)

data KeyEvent = KeyEvent
    { keKey :: GLFW.Key
    , keScanCode :: Int
    , keState :: GLFW.KeyState
    , keModKeys :: GLFW.ModifierKeys
    , keChar :: Maybe Char
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

data EventProcessingState = EventProcessingState
    { epMousePos :: Vector2 Double
    , epFrameBufferSize :: Vector2 Int
    , epWindowSize :: Vector2 Int
    }

translate ::
    [GLFWRawEvent] -> EventProcessingState -> ([Event], EventProcessingState)
translate [] state = ([], state)
translate (x : xs) state =
    case x of
    RawWindowClose -> simple EventWindowClose
    RawWindowRefresh -> simple EventWindowRefresh
    RawDropPaths paths -> simple (EventDropPaths paths)
    RawCharEvent _ ->
        -- Skip char events here as they are processed together with the
        -- key events that immediately precede them.
        translate xs state
    RawKeyEvent key scanCode keyState modKeys ->
        case xs of
        RawCharEvent char : _ -> eventKey (fromChar char)
        _ -> eventKey Nothing
        where
            eventKey ch =
                simple $ EventKey $ KeyEvent key scanCode keyState modKeys ch
    RawFrameBufferSize size ->
        (EventFrameBufferSize size : ne, ns)
        where
            (ne, ns) = translate xs state { epFrameBufferSize = size }
    RawWindowSize size ->
        -- Only change the window position
        translate xs state { epWindowSize = size }
    RawMousePosition newPos ->
        -- Only change the mouse position
        translate xs state { epMousePos = newPos }
    RawMouseButton button buttonState modKeys ->
        simple $ EventMouseButton $
            MouseButtonEvent button buttonState modKeys
            (p * (fromIntegral <$> epFrameBufferSize state)
                / (fromIntegral <$> epWindowSize state))
            p
        where
            p = epMousePos state
    where
        (nextEvents, nextState) = translate xs state
        simple out = (out : nextEvents, nextState)

data EventLoopDisallowedWhenMasked = EventLoopDisallowedWhenMasked
    deriving (Show, Typeable)
instance E.Exception EventLoopDisallowedWhenMasked

eventLoop :: GLFW.Window -> ([Event] -> IO Next) -> IO ()
eventLoop win eventsHandler =
    do
        maskingState <- E.getMaskingState
        when (maskingState /= E.Unmasked) $ E.throwIO EventLoopDisallowedWhenMasked
        eventsVar <- newIORef [RawWindowRefresh]
        eventProcessingStateRef <-
            EventProcessingState
            <$> (uncurry Vector2 <$> GLFW.getCursorPos win)
            <*> (uncurry Vector2 <$> GLFW.getFramebufferSize win)
            <*> (uncurry Vector2 <$> GLFW.getWindowSize win)
            >>= newIORef

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
                    oldState <- readIORef eventProcessingStateRef
                    let (events, newState) = translate rawEvents oldState
                    writeIORef eventProcessingStateRef newState
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
                        NextQuit -> return ()

        setCallback GLFW.setCharCallback (addEvent . RawCharEvent)
        setCallback GLFW.setKeyCallback addKeyEvent
        setCallback GLFW.setMouseButtonCallback addMouseButtonEvent
        setCallback GLFW.setCursorPosCallback $ addVec2Event RawMousePosition
        setCallback GLFW.setDropCallback (addEvent . RawDropPaths)
        setCallback GLFW.setWindowRefreshCallback $ addEvent RawWindowRefresh
        setCallback GLFW.setFramebufferSizeCallback $ addVec2Event RawFrameBufferSize
        setCallback GLFW.setWindowSizeCallback $ addVec2Event RawWindowSize
        setCallback GLFW.setWindowCloseCallback $ addEvent RawWindowClose

        GLFW.swapInterval 1
        loop
