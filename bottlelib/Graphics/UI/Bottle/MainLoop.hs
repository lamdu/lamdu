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
                        do
                            -- TODO: If we can verify that there's sync-to-vblank, we
                            -- need no sleep here
                            threadDelay 10000
                            return False
                    Just image ->
                        do
                            image
                                & (DrawUtils.translate (Vector2 (-1) 1) <>
                                   DrawUtils.scale (Vector2 (2/winSizeX) (-2/winSizeY)) %%)
                                & let Vector2 glPixelRatioX glPixelRatioY = winSize / 2 -- GL range is -1..1
                                  in clearRenderSized (glPixelRatioX, glPixelRatioY)
                            return True

clearRenderSized :: Draw.R2 -> Draw.Image a -> IO ()
#ifdef DRAWINGCOMBINATORS__SIZED
clearRenderSized = Draw.clearRenderSized
#else
clearRenderSized _ = Draw.clearRender
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
                        <&> (_1 .~ True)
                    return True

            nextFrameState curTime handlers Nothing =
                do
                    dest <- animMakeFrame handlers
                    return $ Just (True, (curTime, dest))
            nextFrameState curTime handlers (Just (wasChange, (prevTime, prevFrame))) =
                if wasChange
                then do
                    dest <- animMakeFrame handlers
                    animationHalfLife <- getAnimationHalfLife
                    let elapsed = realToFrac (curTime `diffUTCTime` prevTime)
                        progress = 1 - 0.5 ** (elapsed/animationHalfLife)
                    return . Just $
                        case Anim.nextFrame progress dest prevFrame of
                        Nothing -> (False, (curTime, dest))
                        Just newFrame -> (True, (curTime, newFrame))
                else
                    return $ Just (False, (curTime, prevFrame))

            frameStateResult Nothing = error "No frame to draw at start??"
            frameStateResult (Just (change, (_, frame)))
                | change = Just $ Anim.draw frame
                | otherwise = Nothing
        mainLoopImage win $ \size -> ImageHandlers
            { imageEventHandler = \event ->
                animEventHandler (animHandlers size) event
                >>= handleResult
            , imageMake = \forceRedraw ->
                do
                    let handlers = animHandlers size
                    when forceRedraw .
                        modifyIORef frameStateVar $
                        Lens.mapped . _1 .~ True
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
