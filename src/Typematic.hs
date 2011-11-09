{-# OPTIONS -Wall #-}
module Typematic (typematicKeyHandlerWrap) where

import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Graphics.UI.GLFW
import Data.Time.Clock

data TypematicState = NoKey | TypematicRepeat { _tsKey :: Key, _tsCount :: Int, _tsStartTime :: UTCTime }

typematicKeyHandlerWrap :: (Int -> NominalDiffTime) -> (Key -> Bool -> IO ()) -> IO (Key -> Bool -> IO ())
typematicKeyHandlerWrap timeFunc handler = do
  stateVar <- newMVar NoKey
  _ <- forkIO . forever $ do
    sleepTime <- modifyMVar stateVar typematicIteration
    threadDelay . round $ 1000000 * sleepTime

  return $ \key isPress -> do
    newValue <-
      if isPress
        then fmap (TypematicRepeat key 0) getCurrentTime
        else return NoKey

    _ <- swapMVar stateVar newValue
    handler key isPress

  where
    typematicIteration state@(TypematicRepeat key count startTime) = do
      now <- getCurrentTime
      let timeDiff = diffUTCTime now startTime
      if timeDiff >= timeFunc count
        then do
          handler key True
          return (TypematicRepeat key (count + 1) startTime,
                  timeFunc (count + 1) - timeDiff)
        else
          return (state, timeFunc count - timeDiff)
    typematicIteration state@NoKey = return (state, timeFunc 0)

