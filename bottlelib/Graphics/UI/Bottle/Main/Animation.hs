{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Graphics.UI.Bottle.Main.Animation
    ( AnimConfig(..), Handlers(..), EventResult(..)
    , Looper(..), newLooper
    ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar, swapTVar)
import           Control.Concurrent.Utils (forwardSynchronuousExceptions, withForkedIO)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Exception (evaluate)
import           Control.Monad (when, forever)
import qualified Control.Monad.STM as STM
import           Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Main.Image as MainImage
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event)

import           Prelude.Compat

data IsAnimating
    = Animating NominalDiffTime -- Current animation speed half-life
    | NotAnimating
    deriving Eq

-- Animation thread will have not only the cur frame, but the dest
-- frame in its mutable current state (to update it asynchronously)

-- Worker thread receives events, ticks (which may be lost), handles them, responds to animation thread
-- Animation thread sends events, ticks to worker thread. Samples results from worker thread, applies them to the cur state

data AnimState = AnimState
    { _asIsAnimating :: !IsAnimating
    , _asCurTime :: !UTCTime
    , _asCurFrame :: !Anim.Frame
    , _asDestFrame :: !Anim.Frame
    }
Lens.makeLenses ''AnimState

data EventsData = EventsData
    { _edHaveTicks :: !Bool
    , _edRefreshRequested :: !Bool
    , _edWinSize :: !Anim.Size
    , _edReversedEvents :: [Event]
    }
Lens.makeLenses ''EventsData

-- The threads communicate via these STM variables
data ThreadVars = ThreadVars
    { animStateVar :: TVar AnimState
    , eventsVar :: TVar EventsData
    , runInAnimVar :: TVar (IO ())
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

desiredFrameRate :: Num a => a
desiredFrameRate = 60

initialAnimState :: Anim.Frame -> IO AnimState
initialAnimState initialFrame =
    do
        curTime <- getCurrentTime
        return AnimState
            { _asIsAnimating = NotAnimating
            , _asCurTime = curTime
            , _asCurFrame = initialFrame
            , _asDestFrame = initialFrame
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

eventHandlerThread :: ThreadVars -> IO AnimConfig -> (Anim.Size -> Handlers) -> IO ()
eventHandlerThread tvars getAnimationConfig animHandlers =
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
        STM.atomically $
            modifyTVar (runInAnimVar tvars) (>> erExecuteInMainThread result)
        case (ed ^. edRefreshRequested, erAnimIdMapping result) of
            (False, Nothing) -> return ()
            (_, mMapping) ->
                do
                    destFrame <-
                        makeFrame handlers
                        -- Force destFrame so that we don't get unknown computations
                        -- happening inside STM.atomically modifying the state var.
                        -- Without this we may get nested STM.atomically errors.
                        >>= evaluate
                    AnimConfig timePeriod ratio <- getAnimationConfig
                    curTime <- getCurrentTime
                    let timeRemaining =
                            max 0 $
                            diffUTCTime
                            (addUTCTime timePeriod userEventTime)
                            curTime
                    let animationHalfLife = timeRemaining / realToFrac (logBase 0.5 ratio)
                    STM.atomically $ modifyTVar (animStateVar tvars) $
                        \oldFrameState ->
                        oldFrameState
                        & asIsAnimating .~ Animating animationHalfLife
                        & asDestFrame .~ destFrame
                        -- retroactively pretend animation started a little bit
                        -- sooner so there's already a change in the first frame
                        & asCurTime .~ addUTCTime (-1.0 / desiredFrameRate) curTime
                        & asCurFrame %~ Anim.mapIdentities (Monoid.appEndo (fromMaybe mempty mMapping))
                    -- In case main thread went to sleep (not knowing
                    -- whether to anticipate a tick result), wake it
                    -- up
                    GLFW.postEmptyEvent

animThread :: ThreadVars -> GLFW.Window -> IO ()
animThread tvars win =
    MainImage.mainLoop win $ \size ->
    MainImage.Handlers
    { MainImage.eventHandler = \event -> (edReversedEvents %~ (event :)) & updateTVar
    , MainImage.refresh =
        do
            updateTVar (edRefreshRequested .~ True)
            updateFrameState size <&> fst <&> _asCurFrame <&> Anim.draw
    , MainImage.update = updateFrameState size <&> snd <&> fmap Anim.draw
    }
    where
        updateTVar = STM.atomically . modifyTVar (eventsVar tvars)
        tick size = updateTVar $ (edHaveTicks .~ True) . (edWinSize .~ size)
        updateFrameState size =
            do
                tick size
                curTime <- getCurrentTime
                (result, runInAnim) <- STM.atomically $
                    do
                        animState <- readTVar (animStateVar tvars)
                        let (newAnimState, mFrameToDraw) =
                                case animState ^. asIsAnimating of
                                Animating animationHalfLife ->
                                    case Anim.nextFrame progress destFrame (animState ^. asCurFrame) of
                                    Nothing ->
                                        ( animState
                                            & asCurFrame .~ destFrame
                                            & asIsAnimating .~ NotAnimating
                                        , Just destFrame
                                        )
                                    Just newFrame ->
                                        ( animState & asCurFrame .~ newFrame
                                        , Just newFrame
                                        )
                                    where
                                        destFrame = animState ^. asDestFrame
                                        elapsed = curTime `diffUTCTime` (animState ^. asCurTime)
                                        progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac animationHalfLife)
                                NotAnimating -> (animState, Nothing)
                                & _1 . asCurTime .~ curTime
                        writeTVar (animStateVar tvars) newAnimState
                        runInAnim <- swapTVar (runInAnimVar tvars) (return ())
                        return ((newAnimState, mFrameToDraw), runInAnim)
                runInAnim
                return result

newtype Looper = Looper
    { _runLooper :: GLFW.Window -> IO AnimConfig -> (Anim.Size -> Handlers) -> IO ()
    }

newLooper :: IO Looper
newLooper =
    do
        animState <- initialAnimState mempty >>= newTVarIO
        return $ Looper $ \win getAnimationConfig animHandlers ->
            do
                winSize <- MainImage.windowSize win
                frame <- makeFrame (animHandlers winSize)
                STM.atomically $ modifyTVar animState $ \s -> s
                    & asCurFrame . Anim.unitImages .~ mempty
                    & asDestFrame .~ frame
                tvars <-
                    ThreadVars animState
                    <$> newTVarIO EventsData
                        { _edHaveTicks = False
                        , _edRefreshRequested = False
                        , _edWinSize = winSize
                        , _edReversedEvents = []
                        }
                    <*> newTVarIO (return ())
                eventsThread <-
                    forwardSynchronuousExceptions (eventHandlerThread tvars getAnimationConfig animHandlers)
                withForkedIO eventsThread (animThread tvars win)
