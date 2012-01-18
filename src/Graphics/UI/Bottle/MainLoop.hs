{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.MainLoop (mainLoopAnim, mainLoopImage, mainLoopWidget) where

import Control.Concurrent(forkIO)
import Control.Concurrent.MVar
import Control.Exception(SomeException, try, throwIO)
import Control.Monad(forever)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.StateVar (($=))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators ((%%))
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.Bottle.EventMap (Event)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Widget(Widget)
import Graphics.UI.GLFW (defaultDisplayOptions, getWindowDimensions)
import Graphics.UI.GLFW.Events (GLFWEvent(..), eventLoop)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

-- ^ Returns a function that makes all of the given IO actions execute
-- in the same new thread.
coalsceToThread :: IO (IO () -> IO ())
coalsceToThread = do
  requestVar <- newEmptyMVar
  _ <- forkIO . forever $ do
    (action, responseVar) <- takeMVar requestVar
    result <- try action :: IO (Either SomeException ())
    putMVar responseVar result
  let
    runAction action = do
      responseVar <- newEmptyMVar
      putMVar requestVar (action, responseVar)
      either throwIO return =<< takeMVar responseVar

  return runAction

inAnotherThread :: (IO () -> IO ()) -> IO a -> IO a
inAnotherThread coalesce action = do
  m <- newEmptyMVar
  coalesce (action >>= putMVar m)
  takeMVar m

mainLoopImage :: (Size -> Event -> IO ()) -> (Size -> IO Image) -> IO a
mainLoopImage eventHandler makeImage = GLFWUtils.withGLFW $ do
  decorateIO <- coalsceToThread
  let coalesce = inAnotherThread decorateIO

  GLFWUtils.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    eventHandlerWithSize event =
      (coalesce . (`eventHandler` event)) =<< windowSize
  let
    handleEvent (GLFWKeyEvent keyEvent) =
      eventHandlerWithSize keyEvent
    handleEvent GLFWWindowClose =
      error "Quit" -- TODO: Make close event

    handleEvents events = do
      winSize@(Vector2 winSizeX winSizeY) <- windowSize
      mapM_ handleEvent events
      GL.viewport $=
        (GL.Position 0 0,
         GL.Size (round winSizeX) (round winSizeY))
      Draw.clearRender .
        (Draw.translate (-1, 1) %%) .
        (Draw.scale (1/winSizeX) (-1/winSizeY) %%) =<<
        coalesce (makeImage winSize)
  eventLoop handleEvents

mainLoopAnim ::
  (Size -> Event -> IO ()) -> (Size -> IO Anim.Frame) -> IO a
mainLoopAnim eventHandler makeFrame = do
  frameVar <- newIORef mempty
  let
    makeImage size = do
      dest <- makeFrame size
      prevFrame <- readIORef frameVar
      let frame = Anim.nextFrame dest prevFrame
      writeIORef frameVar frame
      return $ Anim.draw frame
  mainLoopImage eventHandler makeImage

mainLoopWidget :: IO (Widget (IO ())) -> IO a
mainLoopWidget mkWidget =
  mainLoopAnim eventHandler mkImage
  where
    eventHandler size event = do
      widget <- mkWidget
      fromMaybe (return ()) . E.lookup event $ Widget.eventMap widget size
    mkImage size = do
      widget <- mkWidget
      return $ Widget.image widget size
