{-# LANGUAGE TupleSections #-}
module Graphics.UI.GLFW.Events(GLFWEvent(..), KeyEvent(..), IsPress(..), eventLoop) where

import Control.Monad(forever, (<=<))
import Data.IORef
import Data.Traversable (traverse)
import Graphics.UI.GLFW.ModState(ModState, isModifierKey, asModkey, modStateFromModkeySet)
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW

data IsPress = Press | Release
  deriving (Show, Read, Eq, Ord)

-- this is the reification of the callback information:
data GLFWRawEvent = RawCharEvent Char
                  | RawKeyEvent IsPress GLFW.Key
                  | RawWindowRefresh
                  | RawWindowClose
  deriving (Show, Eq)

data KeyEvent = KeyEvent {
  kePress :: IsPress,
  keModState :: ModState,
  keChar :: Maybe Char,
  keKey :: GLFW.Key }
  deriving (Show, Eq)

-- This is the final representation we expose of events:
data GLFWEvent = GLFWKeyEvent KeyEvent
               | GLFWWindowClose
               | GLFWWindowRefresh
  deriving (Show, Eq)

translate :: [(ModState, GLFWRawEvent)] -> [GLFWEvent]
translate [] = []
translate ((_, RawWindowClose) : xs) = GLFWWindowClose : translate xs
translate ((_, RawWindowRefresh) : xs) = GLFWWindowRefresh : translate xs
translate ((modState1, RawKeyEvent isPress1 key) :
           (modState2, RawCharEvent char) : xs)
  | isModifierKey key =
     -- This happens when you press shift while a key is pressed,
     -- ignore
     GLFWKeyEvent (KeyEvent isPress1 modState1 Nothing key) : translate xs
  | otherwise =
    GLFWKeyEvent (KeyEvent isPress1 modState2 (Just char) key) : translate xs
translate ((modState, RawKeyEvent isPress key) : xs) =
  GLFWKeyEvent (KeyEvent isPress modState Nothing key) : translate xs
-- This happens when you press shift while a key is pressed, ignore
translate ((_, RawCharEvent _) : xs) = translate xs

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ var f = atomicModifyIORef var ((, ()) . f)

isPressFromKeyState :: GLFW.KeyState -> IsPress
isPressFromKeyState GLFW.KeyState'Pressed = Press
isPressFromKeyState GLFW.KeyState'Repeating = Press
isPressFromKeyState GLFW.KeyState'Released = Release

rawEventLoop :: GLFW.Window -> ([GLFWRawEvent] -> IO ()) -> IO a
rawEventLoop win eventsHandler = do
  eventsVar <- newIORef []

  let
    addEvent event = atomicModifyIORef_ eventsVar (event:)
    -- TODO: Use given mods instead of manual tracking?
    addKeyEvent key _scanCode isPress _mods =
      addEvent $ RawKeyEvent (isPressFromKeyState isPress) key
    charEventHandler char
      | '\57344' <= char && char <= '\63743' = return () -- Range for "key" characters (keys for left key, right key, etc.)
      | otherwise = addEvent $ RawCharEvent char
    mkCallback = Just . const
  GLFW.setCharCallback win $ mkCallback charEventHandler
  GLFW.setKeyCallback win $ mkCallback addKeyEvent
  GLFW.setWindowRefreshCallback win . mkCallback $ addEvent RawWindowRefresh
  GLFW.setWindowSizeCallback win . mkCallback . const . const $ addEvent RawWindowRefresh
  GLFW.setWindowCloseCallback win . mkCallback $ addEvent RawWindowClose

  forever $ do
    GLFW.pollEvents
    let handleReversedEvents rEvents = ([], reverse rEvents)
    events <- atomicModifyIORef eventsVar handleReversedEvents
    eventsHandler events
    GLFW.swapBuffers win

modKeyHandlerWrap ::
  ([(ModState, GLFWRawEvent)] -> IO ()) ->
  IO ([GLFWRawEvent] -> IO ())
modKeyHandlerWrap handler = do
  keySetVar <- newIORef Set.empty
  let
    modState r = do
      keySet <- readIORef keySetVar
      return (modStateFromModkeySet keySet, r)
    handleEvent e@(RawKeyEvent Press key) = do
      maybe (return ()) (modifyIORef keySetVar . Set.insert) $
        asModkey key
      modState e
    handleEvent e@(RawKeyEvent Release key) = do
      result <- modState e
      maybe (return ()) (modifyIORef keySetVar . Set.delete) $
        asModkey key
      return result
    handleEvent e = modState e
  return $ handler <=< traverse handleEvent

eventLoop :: GLFW.Window -> ([GLFWEvent] -> IO ()) -> IO a
eventLoop win handler = do
  mRawHandler <- modKeyHandlerWrap (handler . translate)
  rawEventLoop win mRawHandler
