{-# OPTIONS -Wall #-}
module MainLoop (mainLoop) where

import Data.Vector.Vector2(Vector2(..))
import EventMap
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.GLFW
import KeyHandlers(modifiersEventHandlerWrap)
import SizeRange (Size)
import Typematic(typematicKeyHandlerWrap)
import qualified GLFWWrap
import qualified Graphics.DrawingCombinators as Draw

mainLoop :: (Size -> Event -> IO ()) -> (Size -> IO Image) -> IO a
mainLoop eventHandler makeImage = GLFWWrap.withGLFW $ do
  GLFWWrap.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    eventHandlerWithSize event = (`eventHandler` event) =<< windowSize
  modifiersHandler <- modifiersEventHandlerWrap eventHandlerWithSize

  let
    keyHandler key isPress = modifiersHandler $ GLFWWrap.KeyEvent key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWWrap.KeyEvent key isPress) = typematicKeyHandler key isPress
    handleEvent GLFWWrap.WindowClose = error "Quit" -- TODO: Make close event
    handleEvent x = modifiersHandler x

  GLFWWrap.eventLoop $ \events -> do
    winSize@(Vector2 winSizeX winSizeY) <- windowSize
    mapM_ handleEvent events
    Draw.clearRender .
      (Draw.scale (20/winSizeX) (-20/winSizeY) %%) =<<
      makeImage winSize

