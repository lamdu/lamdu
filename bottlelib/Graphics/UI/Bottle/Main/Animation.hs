{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Graphics.UI.Bottle.Main.Animation
    ( mainLoop, AnimConfig(..), Handlers(..)
    ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar, modifyTVar)
import           Control.Concurrent.Utils (forwardExceptions, withForkedIO)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
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
    | FinalFrame
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

data ThreadSyncVar = ThreadSyncVar
    { _tsvHaveTicks :: !Bool
    , _tsvRefreshRequested :: !Bool
    , _tsvWinSize :: !Anim.Size
    , _tsvReversedEvents :: [Event]
    }
Lens.makeLenses ''ThreadSyncVar

data AnimConfig = AnimConfig
    { acTimePeriod :: NominalDiffTime
    , acRemainingRatioInPeriod :: Anim.R
    }

data Handlers = Handlers
    { tickHandler :: IO (Maybe (Monoid.Endo AnimId))
    , eventHandler :: Event -> IO (Maybe (Monoid.Endo AnimId))
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

waitForEvent :: TVar ThreadSyncVar -> IO ThreadSyncVar
waitForEvent eventTVar =
    do
        tsv <- readTVar eventTVar
        when (not (tsv ^. tsvHaveTicks) &&
              not (tsv ^. tsvRefreshRequested) &&
              null (tsv ^. tsvReversedEvents))
            STM.retry
        tsv
            & tsvHaveTicks .~ False
            & tsvRefreshRequested .~ False
            & tsvReversedEvents .~ []
            & writeTVar eventTVar
        return tsv
    & STM.atomically

eventHandlerThread :: TVar AnimState -> TVar ThreadSyncVar -> IO AnimConfig -> (Anim.Size -> Handlers) -> IO ()
eventHandlerThread frameStateVar eventTVar getAnimationConfig animHandlers =
    forever $
    do
        tsv <- waitForEvent eventTVar
        userEventTime <- getCurrentTime
        let handlers = animHandlers (tsv ^. tsvWinSize)
        eventResults <-
            mapM (eventHandler handlers) $ reverse (tsv ^. tsvReversedEvents)
        tickResult <-
            if tsv ^. tsvHaveTicks
            then tickHandler handlers
            else return Nothing
        case (tsv ^. tsvRefreshRequested, mconcat (tickResult : eventResults)) of
            (False, Nothing) -> return ()
            (_, mMapping) ->
                do
                    destFrame <- makeFrame handlers
                    AnimConfig timePeriod ratio <- getAnimationConfig
                    curTime <- getCurrentTime
                    let timeRemaining =
                            max 0 $
                            diffUTCTime
                            (addUTCTime timePeriod userEventTime)
                            curTime
                    let animationHalfLife = timeRemaining / realToFrac (logBase 0.5 ratio)
                    STM.atomically $ modifyTVar frameStateVar $
                        \oldFrameState ->
                        oldFrameState
                        & asIsAnimating .~ Animating animationHalfLife
                        & asDestFrame .~ destFrame
                        -- retroactively pretend animation started at
                        -- user event time to make the
                        -- animation-until-dest-frame last the same
                        -- amount of time no matter how long it took
                        -- to handle the event:
                        & asCurTime .~ addUTCTime (-1.0 / desiredFrameRate) curTime
                        & asCurFrame %~ Anim.mapIdentities (Monoid.appEndo (fromMaybe mempty mMapping))
                    -- In case main thread went to sleep (not knowing
                    -- whether to anticipate a tick result), wake it
                    -- up
                    GLFW.postEmptyEvent

animThread :: TVar AnimState -> TVar ThreadSyncVar -> GLFW.Window -> IO ()
animThread frameStateVar eventTVar win =
    MainImage.mainLoop win $ \size ->
    MainImage.Handlers
    { MainImage.eventHandler = \event -> (tsvReversedEvents %~ (event :)) & updateTVar
    , MainImage.refresh =
        do
            updateTVar (tsvRefreshRequested .~ True)
            updateFrameState size <&> _asCurFrame <&> Anim.draw
    , MainImage.update = updateFrameState size <&> frameStateResult
    }
    where
        updateTVar = STM.atomically . modifyTVar eventTVar
        tick size = updateTVar $ (tsvHaveTicks .~ True) . (tsvWinSize .~ size)
        updateFrameState size =
            do
                tick size
                curTime <- getCurrentTime
                STM.atomically $
                    do
                        AnimState prevAnimating prevTime prevFrame destFrame <-
                            readTVar frameStateVar
                        let notAnimating = AnimState NotAnimating curTime destFrame destFrame
                            newAnimState =
                                case prevAnimating of
                                Animating animationHalfLife ->
                                    case Anim.nextFrame progress destFrame prevFrame of
                                    Nothing -> AnimState FinalFrame curTime destFrame destFrame
                                    Just newFrame -> AnimState (Animating animationHalfLife) curTime newFrame destFrame
                                    where
                                        elapsed = curTime `diffUTCTime` prevTime
                                        progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac animationHalfLife)
                                FinalFrame -> notAnimating
                                NotAnimating -> notAnimating
                        writeTVar frameStateVar newAnimState
                        return newAnimState
        frameStateResult (AnimState isAnimating _ frame _) =
            case isAnimating of
            Animating _ -> Just $ Anim.draw frame
            FinalFrame -> Just $ Anim.draw frame
            NotAnimating -> Nothing

mainLoop :: GLFW.Window -> IO AnimConfig -> (Anim.Size -> Handlers) -> IO ()
mainLoop win getAnimationConfig animHandlers =
    do
        initialWinSize <- MainImage.windowSize win
        frameStateVar <-
            makeFrame (animHandlers initialWinSize)
            >>= initialAnimState >>= newTVarIO
        eventTVar <-
            newTVarIO ThreadSyncVar
            { _tsvHaveTicks = False
            , _tsvRefreshRequested = False
            , _tsvWinSize = initialWinSize
            , _tsvReversedEvents = []
            }
        eventsThread <- forwardExceptions (eventHandlerThread frameStateVar eventTVar getAnimationConfig animHandlers)
        withForkedIO eventsThread $ animThread frameStateVar eventTVar win
