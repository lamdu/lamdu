{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Main.Animation
    ( mainLoop
    , AnimConfig(..)
    , Handlers(..)
    , PerfCounters(..)
    , EventLoop.wakeUp
    ) where

import           Control.Concurrent.Extended (rtsSupportsBoundThreads, forwardSynchronuousExceptions, withForkedOS)
import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (onException)
import           Data.IORef
import           Data.Time.Clock (UTCTime, getCurrentTime)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Animation.Engine as Anim
import           GUI.Momentu.Font (Font)
import           GUI.Momentu.Main.Events (Event)
import qualified GUI.Momentu.Main.Events.Loop as EventLoop
import           GUI.Momentu.Main.Types (AnimConfig(..))
import           GUI.Momentu.Render (render, PerfCounters(..))
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

data Handlers = Handlers
    { reportPerfCounters :: PerfCounters -> IO ()
    , getFPSFont :: IO (Maybe Font)
    , getAnimConfig :: IO AnimConfig
    , makeFrame :: IO Anim.Frame
    , tickHandler :: IO Bool
        -- ^ Returns whether the tick was handled, and a frame update
        -- is needed
    , eventHandler :: Event -> IO Bool
        -- ^ Returns whether the event was handled, meaning two things:
        -- 1. A new frame will be generated
        -- 2. Do not pass event to further processing (e.g: input managers)
    }

eventThreadLoop :: (Maybe (UTCTime, Anim.Frame) -> IO ()) -> GLFW.Window -> Handlers -> IO ()
eventThreadLoop sendNewFrame win handlers =
    do
        lastTimestampRef <- newIORef Nothing
        let updateTimestamp act =
                do
                    preTimestamp <- getCurrentTime
                    didAnything <- act
                    when didAnything
                        (writeIORef lastTimestampRef (Just preTimestamp))
                    pure didAnything
        EventLoop.eventLoop win EventLoop.Handlers
            { EventLoop.eventHandler = updateTimestamp . eventHandler handlers
            , EventLoop.iteration =
                do
                    _ <- updateTimestamp (tickHandler handlers)
                    lastTimestamp <- readIORef lastTimestampRef
                    writeIORef lastTimestampRef Nothing
                    traverse_ sendStampedFrame lastTimestamp
                    pure EventLoop.NextWait
            }
    where
        sendStampedFrame timestamp =
            makeFrame handlers <&> (,) timestamp <&> Just
            >>= sendNewFrame

animThreadLoop :: STM (Maybe (UTCTime, Anim.Frame)) -> Handlers -> GLFW.Window -> IO ()
animThreadLoop recvNewFrame handlers win =
    do
        GLFW.makeContextCurrent (Just win)
        initialFrame <- makeFrame handlers
        initialTime <- getCurrentTime
        Anim.initialState >>= loop (Just (initialTime, initialFrame))
    where
        waitNewFrame = STM.atomically $ recvNewFrame >>= maybe STM.retry pure
        pollNewFrame = STM.atomically recvNewFrame
        loop frameReq animState =
            do
                Anim.currentFrame animState & Anim.draw & render win
                    >>= reportPerfCounters handlers
                animConfig <- getAnimConfig handlers
                (nextAnimState, nextFrameReq) <-
                    Anim.clockedAdvanceAnimation animConfig frameReq animState
                    >>= \case
                    Anim.AnimationComplete -> waitNewFrame <&> Just <&> (,) animState
                    Anim.NewState nextAnimState -> pollNewFrame <&> (,) nextAnimState
                loop nextFrameReq nextAnimState

mainLoop :: GLFW.Window -> Handlers -> IO ()
mainLoop win handlers =
    do
        unless rtsSupportsBoundThreads (error "mainLoop requires threaded runtime")
        newFrameReq <- STM.newTVarIO Nothing
        animThread <-
            animThreadLoop (STM.swapTVar newFrameReq Nothing) handlers win
            & forwardSynchronuousExceptions
        withForkedOS
            (animThread `onException` EventLoop.wakeUp)
            (eventThreadLoop (STM.atomically . STM.writeTVar newFrameReq)
                win handlers)
