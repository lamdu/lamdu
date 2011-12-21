{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.MainLoop (mainLoop, mainLoopWidget) where

import Data.Maybe (fromMaybe)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.Bottle.EventMap (Event)
import Graphics.UI.Bottle.KeyHandlers (modifiersEventHandlerWrap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Typematic(typematicKeyHandlerWrap)
import Graphics.UI.Bottle.Widget(Widget)
import Graphics.UI.GLFW (defaultDisplayOptions, getWindowDimensions)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

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

mainLoopWidget :: IO (Widget (IO ())) -> IO a
mainLoopWidget mkWidget = mainLoop eventHandler mkImage
  where
    eventHandler size event = do
      widget <- mkWidget
      fromMaybe (return ()) $ E.lookup event =<< Widget.eventMap widget True size
    mkImage size = do
      widget <- mkWidget
      return $ Widget.image widget True size
