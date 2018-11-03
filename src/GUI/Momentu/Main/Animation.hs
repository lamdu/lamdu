{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.Main.Animation
    ( mainLoop
    , AnimConfig(..)
    , Handlers(..), PerfCounters(..)
    , EventResult(..)
    , MainImage.wakeUp
    ) where

import           Control.Concurrent.Extended (rtsSupportsBoundThreads, forwardSynchronuousExceptions, withForkedIO)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar, swapTVar)
import           Control.DeepSeq (force)
import           Control.Exception (evaluate, onException)
import qualified Control.Lens as Lens
import           Control.Monad (mplus)
import qualified Control.Monad.STM as STM
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Monoid as Monoid
import           Data.Time.Clock (UTCTime, getCurrentTime)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Animation.Engine as Anim
import           GUI.Momentu.Font (Font)
import           GUI.Momentu.Main.Events (Event)
import           GUI.Momentu.Main.Image (PerfCounters(..), TickResult(..))
import qualified GUI.Momentu.Main.Image as MainImage
import           GUI.Momentu.Main.Types (AnimConfig(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           Lamdu.Prelude

-- Animation thread will have not only the cur frame, but the dest
-- frame in its mutable current state (to update it asynchronously)

-- Worker thread receives events, ticks (which may be lost), handles them, responds to animation thread
-- Animation thread sends events, ticks to worker thread. Samples results from worker thread, applies them to the cur state

data EventsData = EventsData
    { _edHaveTicks :: !Bool
    , _edRefreshRequested :: !Bool
    , _edWinSize :: !Anim.Size
    , _edReversedEvents :: [Event]
    } deriving Show
Lens.makeLenses ''EventsData

-- Data sent from the events thread to the anim thread
data ToAnim = ToAnim
    { taEventResult :: EventResult
    , taMNewFrame :: Maybe (UTCTime, Anim.Frame)
    }

instance Semigroup ToAnim where
    -- Newer ToAnim is on the left, taking it's new frame if exists.
    ToAnim erA nfA <> ToAnim erB nfB = ToAnim (erA <> erB) (mplus nfA nfB)
instance Monoid ToAnim where
    mempty = ToAnim mempty Nothing
    mappend = (<>)

-- The threads communicate via these STM variables
data ThreadVars = ThreadVars
    { eventsVar :: TVar EventsData
    , toAnimVar :: TVar ToAnim
    }

newtype EventResult = EventResult
    { erUpdate :: Monoid.Any
    }

instance Semigroup EventResult where
    EventResult am <> EventResult bm = EventResult (am <> bm)
instance Monoid EventResult where
    mempty = EventResult mempty
    mappend = (<>)

data Handlers = Handlers
    { tickHandler :: IO EventResult
    , eventHandler :: Event -> IO EventResult
    , makeFrame :: IO Anim.Frame
    }

waitForEvent :: TVar EventsData -> IO EventsData
waitForEvent eventTVar =
    do
        ed <- readTVar eventTVar
        STM.check
            ((ed ^. edHaveTicks) ||
             (ed ^. edRefreshRequested) ||
             (not . null) (ed ^. edReversedEvents))
        ed
            & edHaveTicks .~ False
            & edRefreshRequested .~ False
            & edReversedEvents .~ []
            & writeTVar eventTVar
        pure ed
    & STM.atomically

eventHandlerThread :: ThreadVars -> (Anim.Size -> Handlers) -> IO ()
eventHandlerThread tvars animHandlers =
    forever $
    do
        ed <- waitForEvent (eventsVar tvars)
        userEventTime <- getCurrentTime
        let handlers = animHandlers (ed ^. edWinSize)
        eventResults <-
            traverse (eventHandler handlers) $ reverse (ed ^. edReversedEvents)
        tickResult <-
            if ed ^. edHaveTicks
            then tickHandler handlers
            else pure mempty
        let result = mconcat (tickResult : eventResults)
        mNewFrame <-
            if ed ^. edRefreshRequested || erUpdate result ^. Lens._Wrapped
            then
                makeFrame handlers
                -- Force destFrame so that we don't get unknown computations
                -- happening inside STM.atomically modifying the state var.
                -- Without this we may get nested STM.atomically errors.
                >>= evaluate . force
                <&> Just
            else pure Nothing
        mappend ToAnim
            { taEventResult = result
            , taMNewFrame = mNewFrame <&> (,) userEventTime
            }
            & modifyTVar (toAnimVar tvars)
            & STM.atomically
        -- In case main thread went to sleep (not knowing whether to anticipate
        -- a tick result), wake it up
        when (Lens.has Lens._Just mNewFrame) MainImage.wakeUp

animThread ::
    (PerfCounters -> IO ()) -> IO (Maybe Font) ->
    ThreadVars -> IORef Anim.State -> IO AnimConfig -> GLFW.Window -> IO ()
animThread reportPerfCounters getFpsFont tvars animStateRef getAnimationConfig win =
    MainImage.mainLoop win $ \size ->
    MainImage.Handlers
    { MainImage.eventHandler =
        \event -> (edReversedEvents %~ (event :)) & updateTVar <&> const True
    , MainImage.refresh =
        do
            updateTVar (edRefreshRequested .~ True)
            _ <- updateFrameState size
            readIORef animStateRef <&> draw
    , MainImage.tick =
        updateFrameState size
        <&> (^? Anim._NewState)
        <&> fmap draw <&> maybe StopTicking TickImage
    , MainImage.fpsFont = getFpsFont
    , MainImage.reportPerfCounters = reportPerfCounters
    }
    where
        draw = Anim.draw . Anim.currentFrame
        updateTVar = STM.atomically . modifyTVar (eventsVar tvars)
        tick size = updateTVar $ (edHaveTicks .~ True) . (edWinSize .~ size)
        updateFrameState size =
            do
                tick size
                fromEvents <- swapTVar (toAnimVar tvars) mempty & STM.atomically
                animConfig <- getAnimationConfig
                mNewState <-
                    readIORef animStateRef
                    >>= Anim.clockedAdvanceAnimation animConfig (taMNewFrame fromEvents)
                _ <-
                    mNewState
                    & Lens.traverseOf_ Anim._NewState (writeIORef animStateRef)
                pure mNewState

mainLoop :: (PerfCounters -> IO ()) -> GLFW.Window -> IO (Maybe Font) -> IO AnimConfig -> (Anim.Size -> Handlers) -> IO ()
mainLoop reportPerfCounters win getFpsFont getAnimationConfig animHandlers =
    do
        unless rtsSupportsBoundThreads (error "mainLoop requires threaded runtime")
        animStateRef <- Anim.initialState >>= newIORef
        initialWinSize <- GLFW.Utils.windowSize win
        tvars <-
            ThreadVars
            <$> newTVarIO EventsData
                { _edHaveTicks = False
                , _edRefreshRequested = False
                , _edWinSize = initialWinSize
                , _edReversedEvents = []
                }
            <*> newTVarIO mempty
        eventsThread <- forwardSynchronuousExceptions (eventHandlerThread tvars animHandlers)
        withForkedIO
            (eventsThread `onException` MainImage.wakeUp)
            (animThread reportPerfCounters
                getFpsFont tvars animStateRef getAnimationConfig win)
