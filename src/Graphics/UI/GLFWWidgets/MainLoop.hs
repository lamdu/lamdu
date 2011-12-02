{-# OPTIONS -Wall #-}
module Graphics.UI.GLFWWidgets.MainLoop (mainLoop) where

import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.GLFWWidgets.EventMap (Event)
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.GLFW (defaultDisplayOptions, getWindowDimensions)
import Graphics.UI.GLFWWidgets.KeyHandlers (modifiersEventHandlerWrap)
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
    keyHandler mchar key isPress = modifiersHandler $ GLFWUtils.KeyEvent mchar key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWUtils.KeyEvent mchar key isPress) = typematicKeyHandler mchar key isPress
    handleEvent GLFWUtils.WindowClose = error "Quit" -- TODO: Make close event

  GLFWUtils.eventLoop $ \events -> do
    winSize@(Vector2 winSizeX winSizeY) <- windowSize
    mapM_ handleEvent events
    Draw.clearRender .
      (Draw.translate (-1, 1) %%) .
      (Draw.scale (1/winSizeX) (-1/winSizeY) %%) =<<
      makeImage winSize
