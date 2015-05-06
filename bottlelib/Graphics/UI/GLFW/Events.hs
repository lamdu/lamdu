{-# LANGUAGE TupleSections #-}
module Graphics.UI.GLFW.Events
  ( KeyEvent(..), Event(..)
  , eventLoop
  ) where

import           Control.Monad (forever, when)
import           Data.IORef
import qualified Graphics.UI.GLFW as GLFW

-- this is the reification of the callback information:
data GLFWRawEvent
    = RawCharEvent Char
    | RawKeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
    | RawWindowRefresh
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
    deriving (Show, Eq)

fromChar :: Char -> Maybe Char
fromChar char
    -- Range for "key" characters (keys for left key, right key, etc.)
    | '\57344' <= char && char <= '\63743' = Nothing
    | otherwise = Just char

translate :: [GLFWRawEvent] -> [Event]
translate [] = []
translate (RawWindowClose : xs) = EventWindowClose : translate xs
translate (RawWindowRefresh : xs) = EventWindowRefresh : translate xs
translate (RawKeyEvent key scanCode keyState modKeys : RawCharEvent char : xs) =
    EventKey (KeyEvent key scanCode keyState modKeys (fromChar char)) :
    translate xs
translate (RawKeyEvent key scanCode keyState modKeys : xs) =
    EventKey (KeyEvent key scanCode keyState modKeys Nothing) : translate xs
translate (RawCharEvent _ : xs) = translate xs

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ var f = atomicModifyIORef var ((, ()) . f)

rawEventLoop :: GLFW.Window -> ([GLFWRawEvent] -> IO Bool) -> IO a
rawEventLoop win eventsHandler = do
  eventsVar <- newIORef []

  let
    addEvent event = atomicModifyIORef_ eventsVar (event:)
    addKeyEvent key scanCode keyState modKeys =
      addEvent $ RawKeyEvent key scanCode keyState modKeys
    charEventHandler = addEvent . RawCharEvent
    setCallback f cb = f win $ Just $ const cb
  setCallback GLFW.setCharCallback charEventHandler
  setCallback GLFW.setKeyCallback addKeyEvent
  setCallback GLFW.setWindowRefreshCallback $ addEvent RawWindowRefresh
  setCallback GLFW.setWindowSizeCallback . const . const $ addEvent RawWindowRefresh
  setCallback GLFW.setWindowCloseCallback $ addEvent RawWindowClose

  forever $ do
    GLFW.pollEvents
    let handleReversedEvents rEvents = ([], reverse rEvents)
    events <- atomicModifyIORef eventsVar handleReversedEvents
    didDraw <- eventsHandler events
    when didDraw $ GLFW.swapBuffers win

eventLoop :: GLFW.Window -> ([Event] -> IO Bool) -> IO a
eventLoop win handler = rawEventLoop win (handler . translate)
