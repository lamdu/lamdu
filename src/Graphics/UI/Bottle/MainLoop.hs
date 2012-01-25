{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.MainLoop (mainLoopAnim, mainLoopImage, mainLoopWidget) where

import Control.Arrow(second)
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception(SomeException, try, throwIO)
import Control.Monad(forever, liftM)
import Data.IORef
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

mainLoopImage :: (Size -> Event -> IO Bool) -> (Bool -> Size -> IO (Maybe Image)) -> IO a
mainLoopImage eventHandler makeImage = GLFWUtils.withGLFW $ do
  decorateIO <- coalsceToThread
  let coalesce = inAnotherThread decorateIO

  GLFWUtils.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    eventHandlerWithSize size event = coalesce $ eventHandler size event
  let
    handleEvent size (GLFWKeyEvent keyEvent) =
      eventHandlerWithSize size keyEvent
    handleEvent _ GLFWWindowClose =
      error "Quit" -- TODO: Make close event
    handleEvent _ GLFWWindowRefresh = return True

    handleEvents events = do
      winSize@(Vector2 winSizeX winSizeY) <- windowSize
      anyChange <- fmap or $ mapM (handleEvent winSize) events
      GL.viewport $=
        (GL.Position 0 0,
         GL.Size (round winSizeX) (round winSizeY))
      mNewImage <- coalesce (makeImage anyChange winSize)
      case mNewImage of
        Nothing -> threadDelay 10000
        Just image ->
          Draw.clearRender .
          (Draw.translate (-1, 1) %%) .
          (Draw.scale (2/winSizeX) (-2/winSizeY) %%) $
          image
  eventLoop handleEvents

mainLoopAnim ::
  (Size -> Event -> IO (Maybe Widget.EventResult)) -> (Size -> IO Anim.Frame) -> IO a
mainLoopAnim eventHandler makeFrame = do
  frameStateVar <- newIORef Nothing
  let
    makeImage isChange size = do
      frameState <- readIORef frameStateVar

      newFrameState <-
        case frameState of
          Nothing -> fmap (Just . (,) True) $ makeFrame size
          Just (wasChange, prevFrame) ->
            if wasChange || isChange
              then do
                dest <- makeFrame size
                return . Just $
                  case Anim.nextFrame dest prevFrame of
                    Nothing -> (False, dest)
                    Just newFrame -> (True, newFrame)
              else
                return $ Just (False, prevFrame)
      writeIORef frameStateVar newFrameState
      return $
        case newFrameState of
          Nothing -> error "No frame to draw at start??"
          Just (change, frame)
            | change -> Just (Anim.draw frame)
            | otherwise -> Nothing
    imgEventHandler size event = do
      mEventResult <- eventHandler size event
      case mEventResult of
        Nothing -> return False
        Just eventResult -> do
          modifyIORef frameStateVar . fmap . second . Anim.mapIdentities $ Widget.eAnimIdMapping eventResult
          return True
  mainLoopImage imgEventHandler makeImage

mainLoopWidget :: IO (Widget IO) -> IO a
mainLoopWidget mkWidget =
  mainLoopAnim eventHandler mkImage
  where
    eventHandler size event = do
      widget <- mkWidget
      maybe (return Nothing) (liftM Just) .
        E.lookup event $ Widget.eventMap widget size
    mkImage size = do
      widget <- mkWidget
      return $ Widget.image widget size
