{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Graphics.UI.Bottle.Main.Animation
    ( AnimConfig(..), Handlers(..), EventResult(..)
    , Looper(..), newLooper
    ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar, swapTVar)
import           Control.Concurrent.Utils (forwardSynchronuousExceptions, withForkedIO)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Exception (evaluate)
import           Control.Monad (mplus, when, forever)
import qualified Control.Monad.STM as STM
import           Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import           Data.Maybe.Utils (unsafeUnjust)
import qualified Data.Monoid as Monoid
import           Data.Monoid ((<>))
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Main.Image as MainImage
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event)

import           Prelude.Compat

-- Animation thread will have not only the cur frame, but the dest
-- frame in its mutable current state (to update it asynchronously)

-- Worker thread receives events, ticks (which may be lost), handles them, responds to animation thread
-- Animation thread sends events, ticks to worker thread. Samples results from worker thread, applies them to the cur state

data AnimState = AnimState
    { _asCurSpeedHalfLife :: !NominalDiffTime
    , _asCurTime :: !UTCTime
    , _asState :: !Anim.State
    }
Lens.makeLenses ''AnimState

data EventsData = EventsData
    { _edHaveTicks :: !Bool
    , _edRefreshRequested :: !Bool
    , _edWinSize :: !Anim.Size
    , _edReversedEvents :: [Event]
    }
Lens.makeLenses ''EventsData

-- Data sent from the events thread to the anim thread
data ToAnim = ToAnim
    { taEventResult :: EventResult
    , taMNewFrame :: Maybe (UTCTime, Anim.Frame)
    }

instance Monoid ToAnim where
    mempty = ToAnim mempty Nothing
    -- Newer ToAnim is on the left, taking it's new frame if exists.
    mappend (ToAnim erA nfA) (ToAnim erB nfB) =
        ToAnim (erA <> erB) (mplus nfA nfB)

-- The threads communicate via these STM variables
data ThreadVars = ThreadVars
    { eventsVar :: TVar EventsData
    , toAnimVar :: TVar ToAnim
    }

data AnimConfig = AnimConfig
    { acTimePeriod :: NominalDiffTime
    , acRemainingRatioInPeriod :: Anim.R
    }

data EventResult = EventResult
    { erAnimIdMapping :: Maybe (Monoid.Endo AnimId)
    , erExecuteInMainThread :: IO ()
    }

instance Monoid EventResult where
    mempty = EventResult mempty (return ())
    mappend (EventResult am ar) (EventResult bm br) =
        EventResult (mappend am bm) (ar >> br)

data Handlers = Handlers
    { tickHandler :: IO EventResult
    , eventHandler :: Event -> IO EventResult
    , makeFrame :: IO Anim.Frame
    }

desiredFrameRate :: Double
desiredFrameRate = 60

initialAnimState :: IO AnimState
initialAnimState =
    do
        curTime <- getCurrentTime
        return AnimState
            { _asCurSpeedHalfLife = 0
            , _asCurTime = curTime
            , _asState = Anim.initialState
            }

waitForEvent :: TVar EventsData -> IO EventsData
waitForEvent eventTVar =
    do
        ed <- readTVar eventTVar
        when (not (ed ^. edHaveTicks) &&
              not (ed ^. edRefreshRequested) &&
              null (ed ^. edReversedEvents))
            STM.retry
        ed
            & edHaveTicks .~ False
            & edRefreshRequested .~ False
            & edReversedEvents .~ []
            & writeTVar eventTVar
        return ed
    & STM.atomically

eventHandlerThread :: ThreadVars -> (Anim.Size -> Handlers) -> IO ()
eventHandlerThread tvars animHandlers =
    forever $
    do
        ed <- waitForEvent (eventsVar tvars)
        userEventTime <- getCurrentTime
        let handlers = animHandlers (ed ^. edWinSize)
        eventResults <-
            mapM (eventHandler handlers) $ reverse (ed ^. edReversedEvents)
        tickResult <-
            if ed ^. edHaveTicks
            then tickHandler handlers
            else return mempty
        let result = mconcat (tickResult : eventResults)
        mNewFrame <-
            if ed ^. edRefreshRequested || Lens.has Lens._Just (erAnimIdMapping result)
            then
                makeFrame handlers
                -- Force destFrame so that we don't get unknown computations
                -- happening inside STM.atomically modifying the state var.
                -- Without this we may get nested STM.atomically errors.
                >>= evaluate
                <&> Just
            else return Nothing
        mappend ToAnim
            { taEventResult = result
            , taMNewFrame = mNewFrame <&> (,) userEventTime
            }
            & modifyTVar (toAnimVar tvars)
            & STM.atomically
        -- In case main thread went to sleep (not knowing whether to anticipate
        -- a tick result), wake it up
        when (Lens.has Lens._Just mNewFrame) GLFW.postEmptyEvent

animThread :: ThreadVars -> IORef AnimState -> IO AnimConfig -> GLFW.Window -> IO ()
animThread tvars animStateRef getAnimationConfig win =
    MainImage.mainLoop win $ \size ->
    MainImage.Handlers
    { MainImage.eventHandler = \event -> (edReversedEvents %~ (event :)) & updateTVar
    , MainImage.refresh =
        do
            updateTVar (edRefreshRequested .~ True)
            _ <- updateFrameState size
            readIORef animStateRef <&> draw
    , MainImage.update = updateFrameState size <&> fmap draw
    }
    where
        draw = Anim.draw . Anim.currentFrame . _asState
        updateTVar = STM.atomically . modifyTVar (eventsVar tvars)
        tick size = updateTVar $ (edHaveTicks .~ True) . (edWinSize .~ size)
        advanceAnimation elapsed mNewDestFrame animState =
            Anim.nextState progress mNewDestFrame (animState ^. asState)
            <&> \newState -> animState & asState .~ newState
            where
                progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac (animState ^. asCurSpeedHalfLife))
        updateFrameState size =
            do
                tick size
                fromEvents <- swapTVar (toAnimVar tvars) mempty & STM.atomically
                taEventResult fromEvents & erExecuteInMainThread
                AnimConfig timePeriod ratio <- getAnimationConfig
                curTime <- getCurrentTime
                mNewState <-
                    readIORef animStateRef <&>
                    \animState ->
                    case taMNewFrame fromEvents of
                    Just (userEventTime, newDestFrame) ->
                        animState
                        & asCurSpeedHalfLife .~ timeRemaining / realToFrac (logBase 0.5 ratio)
                        & asState %~
                            case erAnimIdMapping (taEventResult fromEvents) of
                            Nothing -> id
                            Just mapping -> Anim.stateMapIdentities (Monoid.appEndo mapping)
                        & advanceAnimation elapsed (Just newDestFrame)
                        where
                            -- Retroactively pretend animation started a little bit
                            -- sooner so there's already a change in the first frame
                            elapsed = 1.0 / desiredFrameRate
                            timeRemaining =
                                max 0 $
                                diffUTCTime
                                (addUTCTime timePeriod userEventTime)
                                curTime
                    Nothing ->
                        advanceAnimation (curTime `diffUTCTime` (animState ^. asCurTime)) Nothing animState
                    <&> asCurTime .~ curTime
                _ <- Lens._Just (writeIORef animStateRef) mNewState
                return mNewState

newtype Looper = Looper
    { _runLooper :: GLFW.Window -> IO AnimConfig -> (Anim.Size -> Handlers) -> IO ()
    }

newLooper :: IO Looper
newLooper =
    do
        animStateRef <- initialAnimState >>= newIORef
        return $ Looper $ \win getAnimationConfig animHandlers ->
            do
                winSize <- MainImage.windowSize win
                frame <- makeFrame (animHandlers winSize)
                AnimConfig timePeriod ratio <- getAnimationConfig
                curTime <- getCurrentTime
                modifyIORef animStateRef $
                    \s ->
                    s
                    & asCurTime .~ curTime
                    & asCurSpeedHalfLife .~ timePeriod / realToFrac (logBase 0.5 ratio)
                    & asState %~ Anim.stateClearImages
                    & asState %~
                        unsafeUnjust "nextState with frame always updates state" .
                        Anim.nextState 0 (Just frame)
                tvars <-
                    ThreadVars
                    <$> newTVarIO EventsData
                        { _edHaveTicks = False
                        , _edRefreshRequested = False
                        , _edWinSize = winSize
                        , _edReversedEvents = []
                        }
                    <*> newTVarIO mempty
                eventsThread <-
                    forwardSynchronuousExceptions (eventHandlerThread tvars animHandlers)
                withForkedIO eventsThread (animThread tvars animStateRef getAnimationConfig win)
