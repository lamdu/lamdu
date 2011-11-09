{-# OPTIONS -Wall #-}
module KeyHandlers (modifiersEventHandlerWrap) where

import Control.Monad
import Data.Set(Set)
import qualified Data.Set as Set
import EventMap
import qualified GLFWWrap
import Graphics.UI.GLFW
import Data.IORef

modStateFromKeySet :: Set Key -> ModState
modStateFromKeySet keySet =
  ModState {
    modCtrl = isPressed [KeyLeftCtrl, KeyRightCtrl],
    modMeta = False, -- TODO: GLFW doesn't support meta/winkey?
    modAlt = isPressed [KeyLeftAlt, KeyRightAlt],
    modShift = isPressed [KeyLeftShift, KeyRightShift]
    }
  where
    isPressed = any (`Set.member` keySet)

modifiersEventHandlerWrap :: (Event -> IO ()) -> IO (GLFWWrap.GLFWEvent -> IO ())
modifiersEventHandlerWrap wrappedHandler = do
  keySetVar <- newIORef Set.empty
  let
    handler (GLFWWrap.KeyEvent key True) = do
      modifyIORef keySetVar (Set.insert key)
      keySet <- readIORef keySetVar
      wrappedHandler $ KeyEvent (modStateFromKeySet keySet) key
    handler (GLFWWrap.KeyEvent key False) =
      modifyIORef keySetVar (Set.delete key)
    handler (GLFWWrap.CharEvent char True) = do
      keySet <- readIORef keySetVar
      when (modStateFromKeySet keySet `elem` [noMods, shift]) . wrappedHandler $ CharEvent char
    handler _ = return ()
  return handler

