{-# OPTIONS -Wall #-}
module Graphics.UI.GLFWWidgets.MainLoop (mainLoop) where

import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.GLFWWidgets.EventMap
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.GLFW
import Graphics.UI.GLFWWidgets.KeyHandlers(modifiersEventHandlerWrap)
import Graphics.UI.GLFWWidgets.SizeRange (Size)
import Graphics.UI.GLFWWidgets.Typematic(typematicKeyHandlerWrap)
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import qualified Graphics.DrawingCombinators as Draw

mainLoop :: (Size -> Event -> IO ()) -> (Size -> IO Image) -> IO a
mainLoop eventHandler makeImage = GLFWUtils.withGLFW $ do
  GLFWUtils.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    eventHandlerWithSize event = (`eventHandler` event) =<< windowSize
  modifiersHandler <- modifiersEventHandlerWrap eventHandlerWithSize

  let
    keyHandler key isPress = modifiersHandler $ GLFWUtils.KeyEvent key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWUtils.KeyEvent key isPress) = typematicKeyHandler key isPress
    handleEvent GLFWUtils.WindowClose = error "Quit" -- TODO: Make close event
    handleEvent x = modifiersHandler x

  GLFWUtils.eventLoop $ \events -> do
    winSize@(Vector2 winSizeX winSizeY) <- windowSize
    mapM_ handleEvent events
    Draw.clearRender .
      (Draw.scale (20/winSizeX) (-20/winSizeY) %%) =<<
      makeImage winSize

