{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.GLFW.Events
    ( KeyEvent(..), Event(..), Result(..)
    , eventLoop
    ) where

import           Data.IORef
import           Data.IORef.Utils (atomicModifyIORef_)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW

import           Prelude.Compat

-- this is the reification of the callback information:
data GLFWRawEvent
    = RawCharEvent Char
    | RawKeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
    | RawWindowRefresh
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

-- This is the final representation we expose of events:
data Event
    = EventKey KeyEvent
    | EventWindowClose
    | EventWindowRefresh
    | EventFrameBufferSize (Vector2 Int)
    deriving (Show, Eq)

data Result
    = ResultNone
    | ResultDidDraw
    | ResultQuit
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
translate (RawWindowClose : xs) = EventWindowClose : translate xs
translate (RawWindowRefresh : xs) = EventWindowRefresh : translate xs
translate (RawFrameBufferSize size : xs) = EventFrameBufferSize size : translate xs
translate (RawKeyEvent key scanCode keyState modKeys : RawCharEvent char : xs) =
    EventKey (KeyEvent key scanCode keyState modKeys (fromChar char)) :
    translate xs
translate (RawKeyEvent key scanCode keyState modKeys : xs) =
    EventKey (KeyEvent key scanCode keyState modKeys Nothing) : translate xs
translate (RawCharEvent _ : xs) = translate xs

rawEventLoop :: GLFW.Window -> ([GLFWRawEvent] -> IO Result) -> IO ()
rawEventLoop win eventsHandler =
    do
        eventsVar <- newIORef [RawWindowRefresh]

        let addEvent event = atomicModifyIORef_ eventsVar (event:)
            addKeyEvent key scanCode keyState modKeys =
                addEvent $ RawKeyEvent key scanCode keyState modKeys
            charEventHandler = addEvent . RawCharEvent
            setCallback f cb = f win $ Just $ const cb
            loop =
                do
                    let handleReversedEvents rEvents = ([], reverse rEvents)
                    events <- atomicModifyIORef eventsVar handleReversedEvents
                    res <- eventsHandler events
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

        setCallback GLFW.setCharCallback charEventHandler
        setCallback GLFW.setKeyCallback addKeyEvent
        setCallback GLFW.setWindowRefreshCallback $ addEvent RawWindowRefresh
        setCallback GLFW.setFramebufferSizeCallback $ \w h -> addEvent (RawFrameBufferSize (Vector2 w h))
        setCallback GLFW.setWindowCloseCallback $ addEvent RawWindowClose

        GLFW.swapInterval 1
        loop

eventLoop :: GLFW.Window -> ([Event] -> IO Result) -> IO ()
eventLoop win handler = rawEventLoop win (handler . translate)
