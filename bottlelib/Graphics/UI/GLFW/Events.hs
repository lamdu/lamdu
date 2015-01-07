{-# LANGUAGE TupleSections #-}
module Graphics.UI.GLFW.Events
  ( KeyEvent(..), GLFWEvent(..)
  , eventLoop
  ) where

import           Control.Monad (forever)
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
data GLFWEvent
    = GLFWKeyEvent KeyEvent
    | GLFWWindowClose
    | GLFWWindowRefresh
    deriving (Show, Eq)

fromChar :: Char -> Maybe Char
fromChar char
    -- Range for "key" characters (keys for left key, right key, etc.)
    | '\57344' <= char && char <= '\63743' = Nothing
    | otherwise = Just char

translate :: [GLFWRawEvent] -> [GLFWEvent]
translate [] = []
translate (RawWindowClose : xs) = GLFWWindowClose : translate xs
translate (RawWindowRefresh : xs) = GLFWWindowRefresh : translate xs
translate (RawKeyEvent key scanCode keyState modKeys : RawCharEvent char : xs) =
    GLFWKeyEvent (KeyEvent key scanCode keyState modKeys (fromChar char)) :
    translate xs
translate (RawKeyEvent key scanCode keyState modKeys : xs) =
    GLFWKeyEvent (KeyEvent key scanCode keyState modKeys Nothing) : translate xs
translate (RawCharEvent _ : xs) = translate xs

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ var f = atomicModifyIORef var ((, ()) . f)

rawEventLoop :: GLFW.Window -> ([GLFWRawEvent] -> IO ()) -> IO a
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
    eventsHandler events
    GLFW.swapBuffers win

eventLoop :: GLFW.Window -> ([GLFWEvent] -> IO ()) -> IO a
eventLoop win handler = rawEventLoop win (handler . translate)
