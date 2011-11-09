{-# OPTIONS -Wall #-}
module Graphics.UI.GLFW.Utils(GLFWEvent(..), withGLFW, eventLoop, openWindow) where

import Control.Concurrent(threadDelay)
import Control.Exception(bracket_)
import Control.Monad(forever, unless)
import Data.IORef(newIORef, atomicModifyIORef, modifyIORef)
import qualified Graphics.UI.GLFW as GLFW

data GLFWEvent = CharEvent Char Bool
               | KeyEvent GLFW.Key Bool
               | WindowClose
  deriving (Show, Eq)

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

withGLFW :: IO a -> IO a
withGLFW = bracket_ (GLFW.initialize >>= assert "initialize failed") GLFW.terminate

openWindow :: GLFW.DisplayOptions -> IO ()
openWindow options = GLFW.openWindow options >>= assert "Open window failed"

eventLoop :: ([GLFWEvent] -> IO ()) -> IO b
eventLoop iteration = do
  eventsVar <- newIORef []

  let
    addEvent = modifyIORef eventsVar . (:)
    addKeyEvent c k = addEvent $ KeyEvent c k
    charEventHandler c
      | '\57344' <= c && c <= '\63743' = const $ return () -- Range for "key" characters (keys for left key, right key, etc.)
      | otherwise = addEvent . CharEvent c

  GLFW.setCharCallback charEventHandler
  GLFW.setKeyCallback addKeyEvent
  GLFW.setWindowCloseCallback $ addEvent WindowClose >> return True

  forever $ do
    GLFW.pollEvents
    events <- atomicModifyIORef eventsVar (\o -> ([], reverse o))
    iteration events
    GLFW.swapBuffers
    threadDelay 10000
