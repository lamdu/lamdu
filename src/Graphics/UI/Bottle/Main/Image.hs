-- | A main-loop that draws DrawingCombinator images and reacts to GLFW events,
-- given a Handlers record from the user.
--
-- The higher level Graphics.UI.Bottle.MainLoop builds upon this main-loop.

{-# LANGUAGE NoImplicitPrelude #-}

module Graphics.UI.Bottle.Main.Image
    ( mainLoop, Handlers(..)
    , windowSize
    ) where

import           Control.Lens.Operators
import           Data.Vector.Vector2 (Vector2(..))
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Graphics.DrawingCombinators.Utils (Image)
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Bottle.Animation (Size)
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event, Next(..), eventLoop)
import qualified Graphics.UI.GLFW.Events as GLFWEvents

import           Prelude.Compat

data Handlers = Handlers
    { eventHandler :: Event -> IO ()
    , update :: IO (Maybe Image)
    , refresh :: IO Image
    }

data EventResult =
    ERNone | ERRefresh | ERQuit
    deriving (Eq, Ord, Show)
instance Monoid EventResult where
    mempty = ERNone
    mappend = max

windowSize :: GLFW.Window -> IO Size
windowSize win =
    do
        (x, y) <- GLFW.getFramebufferSize win
        return $ fromIntegral <$> Vector2 x y

mainLoop :: GLFW.Window -> (Size -> Handlers) -> IO ()
mainLoop win imageHandlers =
    do
        initialSize <- windowSize win
        frameBufferSize <- newIORef initialSize
        drawnImageHandlers <- imageHandlers initialSize & newIORef
        let handleEvent GLFWEvents.EventWindowClose = return ERQuit
            handleEvent GLFWEvents.EventWindowRefresh = return ERRefresh
            handleEvent (GLFWEvents.EventFrameBufferSize size) =
                do
                    writeIORef frameBufferSize (fromIntegral <$> size)
                    return ERRefresh
            handleEvent event =
                do
                    handlers <- readIORef drawnImageHandlers
                    eventHandler handlers event
                    return ERNone
        let handleEvents events =
                do
                    eventResult <- mconcat <$> traverse handleEvent events
                    winSize <- readIORef frameBufferSize
                    let handlers = imageHandlers winSize
                    writeIORef drawnImageHandlers handlers
                    case eventResult of
                        ERQuit -> return NextQuit
                        ERRefresh -> refresh handlers >>= draw winSize
                        ERNone ->
                            update handlers >>=
                            maybe (return NextWait) (draw winSize)
        eventLoop win handleEvents
    where
        draw (Vector2 winSizeX winSizeY) image =
            do
                GL.viewport $=
                    (GL.Position 0 0,
                     GL.Size (round winSizeX) (round winSizeY))
                GL.matrixMode $= GL.Projection
                GL.loadIdentity
                GL.ortho 0 winSizeX winSizeY 0 (-1) 1
                DrawUtils.clearRender image
                GLFW.swapBuffers win
                return NextPoll
