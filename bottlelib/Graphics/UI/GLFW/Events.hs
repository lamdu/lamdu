-- | Event-loop for GLFW (instead of its native callbacks model).

{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.GLFW.Events
    ( eventLoop
    , Event(..), KeyEvent(..)
    , Result(..)
    ) where

import qualified Control.Exception as E
import           Control.Monad (when)
import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import           Data.Typeable (Typeable)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW

import           Prelude.Compat

-- GLFWRawEvent is the reification of the callback information.
-- It differs from Event in that in some cases events are grouped together
-- (i.e a RawKeyEvent and a RawCharEvent become a combined KeyEvent).
data GLFWRawEvent
    = RawCharEvent Char
    | RawKeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
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
    , keChar :: Maybe Char
    } deriving (Show, Eq)

data Event
    = EventKey KeyEvent
    | EventWindowClose
    | EventWindowRefresh
    | EventDropPaths [FilePath]
    | EventFrameBufferSize (Vector2 Int)
    deriving (Show, Eq)

-- | The output of the event handler back to the event-loop.
data Result
    = ResultNone
    | ResultDidDraw
    -- ^ The event handler performed drawing
    --   (so a `GLFW.swapBuffers` call is necessary)
    | ResultQuit
    -- ^ The event-loop should quit.
    deriving (Show, Eq, Ord)

instance Monoid Result where
    mempty = ResultNone
    mappend = max

fromChar :: Char -> Maybe Char
fromChar char
    -- Range for "key" characters (keys for left key, right key, etc.)
    | '\57344' <= char && char <= '\63743' = Nothing
    | otherwise = Just char

translate :: [GLFWRawEvent] -> [Event]
translate [] = []
translate (x : xs) =
    case x of
    RawWindowClose -> EventWindowClose : ys
    RawWindowRefresh -> EventWindowRefresh : ys
    RawDropPaths paths -> EventDropPaths paths : ys
    RawFrameBufferSize size -> EventFrameBufferSize size : ys
    RawCharEvent _ -> ys
    RawKeyEvent key scanCode keyState modKeys ->
        case xs of
        RawCharEvent char : xss -> eventKey (fromChar char) : translate xss
        _ -> eventKey Nothing : ys
        where
            eventKey ch = EventKey (KeyEvent key scanCode keyState modKeys ch)
    where
        ys = translate xs

data EventLoopDisallowedWhenMasked = EventLoopDisallowedWhenMasked
    deriving (Show, Typeable)
instance E.Exception EventLoopDisallowedWhenMasked

eventLoop :: GLFW.Window -> ([Event] -> IO Result) -> IO ()
eventLoop win eventsHandler =
    do
        maskingState <- E.getMaskingState
        when (maskingState /= E.Unmasked) $ E.throwIO EventLoopDisallowedWhenMasked
        eventsVar <- newIORef [RawWindowRefresh]

        let addEvent event = atomicModifyIORef_ eventsVar (event:)
            addKeyEvent key scanCode keyState modKeys =
                addEvent $ RawKeyEvent key scanCode keyState modKeys
            setCallback f cb = f win $ Just $ const cb
            loop =
                do
                    let handleReversedEvents rEvents = ([], reverse rEvents)
                    events <- atomicModifyIORef eventsVar handleReversedEvents
                    res <- eventsHandler (translate events)
                    case res of
                        ResultNone ->
                            do
                                GLFW.waitEvents
                                loop
                        ResultDidDraw ->
                            do
                                GLFW.swapBuffers win
                                GLFW.pollEvents
                                loop
                        ResultQuit -> return ()

        setCallback GLFW.setCharCallback (addEvent . RawCharEvent)
        setCallback GLFW.setKeyCallback addKeyEvent
        setCallback GLFW.setDropCallback (addEvent . RawDropPaths)
        setCallback GLFW.setWindowRefreshCallback $ addEvent RawWindowRefresh
        setCallback GLFW.setFramebufferSizeCallback $ \w h -> addEvent (RawFrameBufferSize (Vector2 w h))
        setCallback GLFW.setWindowCloseCallback $ addEvent RawWindowClose

        GLFW.swapInterval 1
        loop
