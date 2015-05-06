{-# LANGUAGE CPP #-}
module Graphics.UI.Bottle.MainLoop
    ( mainLoopAnim
    , mainLoopImage
    , mainLoopWidget
    ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (when, unless)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Monoid as Monoid
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Data.Traversable (traverse, sequenceA)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (Image)
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (KeyEvent, Event(..), eventLoop)

type ForceRedraw = Bool

data ImageHandlers = ImageHandlers
  { imageEventHandler :: KeyEvent -> IO Bool
  , imageMake :: ForceRedraw -> IO (Maybe Image)
  }

mainLoopImage :: GLFW.Window -> (Widget.Size -> ImageHandlers) -> IO a
mainLoopImage win imageHandlers =
    eventLoop win handleEvents
    where
        windowSize =
            do
                (x, y) <- GLFW.getFramebufferSize win
                return $ fromIntegral <$> Vector2 x y

        handleEvent handlers (EventKey keyEvent) =
            imageEventHandler handlers keyEvent
        handleEvent _ EventWindowClose =
            error "Quit" -- TODO: Make close event
        handleEvent _ EventWindowRefresh = return True

        handleEvents events =
            do
                winSize@(Vector2 winSizeX winSizeY) <- windowSize
                let handlers = imageHandlers winSize
                anyChange <- or <$> traverse (handleEvent handlers) events
                -- TODO: Don't do this *EVERY* frame but on frame-buffer size update events?
                GL.viewport $=
                    (GL.Position 0 0,
                     GL.Size (round winSizeX) (round winSizeY))
                mNewImage <- imageMake handlers anyChange
                case mNewImage of
                    Nothing ->
                        -- TODO: If we can verify that there's sync-to-vblank, we
                        -- need no sleep here
                        threadDelay 10000
                    Just image ->
                        image
                        & (DrawUtils.translate (Vector2 (-1) 1) <>
                           DrawUtils.scale (Vector2 (2/winSizeX) (-2/winSizeY)) %%)
#ifdef DRAWINGCOMBINATORS__SIZED
                        & let Vector2 glPixelRatioX glPixelRatioY = winSize / 2 -- GL range is -1..1
                          in Draw.clearRenderSized (glPixelRatioX, glPixelRatioY)
#else
                        & Draw.clearRender
#endif

data AnimHandlers = AnimHandlers
    { animTickHandler :: IO (Maybe (Monoid.Endo AnimId))
    , animEventHandler :: KeyEvent -> IO (Maybe (Monoid.Endo AnimId))
    , animMakeFrame :: IO Anim.Frame
    }

mainLoopAnim :: GLFW.Window -> IO Anim.R -> (Widget.Size -> AnimHandlers) -> IO a
mainLoopAnim win getAnimationHalfLife animHandlers =
    do
        frameStateVar <- newIORef Nothing
        let handleResult Nothing = return False
            handleResult (Just animIdMapping) =
                do
                    modifyIORef frameStateVar $ \state ->
                        state
                        <&> (_2 . _2 %~ Anim.mapIdentities (Monoid.appEndo animIdMapping))
                        <&> (_1 .~ 0)
                    return True

            nextFrameState curTime handlers Nothing =
                do
                    dest <- animMakeFrame handlers
                    return $ Just (0, (curTime, dest))
            nextFrameState curTime handlers (Just (drawCount, (prevTime, prevFrame))) =
                if drawCount == 0
                then do
                    dest <- animMakeFrame handlers
                    animationHalfLife <- getAnimationHalfLife
                    let elapsed = realToFrac (curTime `diffUTCTime` prevTime)
                        progress = 1 - 0.5 ** (elapsed/animationHalfLife)
                    return . Just $
                        case Anim.nextFrame progress dest prevFrame of
                        Nothing -> (drawCount + 1, (curTime, dest))
                        Just newFrame -> (0 :: Int, (curTime, newFrame))
                else
                    return $ Just (drawCount + 1, (curTime, prevFrame))

            frameStateResult Nothing = error "No frame to draw at start??"
            frameStateResult (Just (drawCount, (_, frame)))
                | drawCount < stopAtDrawCount = Just $ Anim.draw frame
                | otherwise = Nothing
            -- A note on draw counts:
            -- When a frame is dis-similar to the previous the count resets to 0
            -- When a frame is similar and animation stops the count becomes 1
            -- We then should draw it again (for double buffering issues) at count 2
            -- And stop drawing it at count 3.
            stopAtDrawCount = 3
        mainLoopImage win $ \size -> ImageHandlers
            { imageEventHandler = \event ->
                animEventHandler (animHandlers size) event
                >>= handleResult
            , imageMake = \forceRedraw ->
                do
                    let handlers = animHandlers size
                    when forceRedraw .
                        modifyIORef frameStateVar $
                        Lens.mapped . _1 .~ 0
                    _ <- handleResult =<< animTickHandler handlers
                    curTime <- getCurrentTime
                    writeIORef frameStateVar =<<
                        nextFrameState curTime handlers =<<
                        readIORef frameStateVar
                    frameStateResult <$> readIORef frameStateVar
            }

mainLoopWidget :: GLFW.Window -> IO Bool -> (Widget.Size -> IO (Widget IO)) -> IO Anim.R -> IO a
mainLoopWidget win widgetTickHandler mkWidgetUnmemod getAnimationHalfLife =
    do
        mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
        let newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
            getWidget size = ($ size) =<< readIORef mkWidgetRef
        mainLoopAnim win getAnimationHalfLife $ \size -> AnimHandlers
            { animTickHandler =
                do
                    anyUpdate <- widgetTickHandler
                    when anyUpdate newWidget
                    widget <- getWidget size
                    tickResults <-
                        sequenceA (widget ^. Widget.eventMap . E.emTickHandlers)
                    unless (null tickResults) newWidget
                    return $
                        case (tickResults, anyUpdate) of
                        ([], False) -> Nothing
                        _ -> Just . mconcat $ map (^. Widget.eAnimIdMapping) tickResults
            , animEventHandler = \event ->
                do
                    widget <- getWidget size
                    mAnimIdMapping <-
                        (traverse . fmap) (^. Widget.eAnimIdMapping) .
                        E.lookup event $ widget ^. Widget.eventMap
                    case mAnimIdMapping of
                        Nothing -> return ()
                        Just _ -> newWidget
                    return mAnimIdMapping
            , animMakeFrame = getWidget size <&> (^. Widget.animFrame)
            }
