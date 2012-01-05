{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Typematic (typematicKeyHandlerWrap) where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, modifyMVar, swapMVar)
import Graphics.UI.GLFW (Key)
import Data.Time.Clock (UTCTime, getCurrentTime, NominalDiffTime, diffUTCTime)

data TypematicState = NoKey | TypematicRepeat { _tsChar :: Maybe Char, _tsKey :: Key, _tsCount :: Int, _tsStartTime :: UTCTime }

typematicKeyHandlerWrap :: (Int -> NominalDiffTime) -> (Maybe Char -> Key -> Bool -> IO ()) -> IO (Maybe Char -> Key -> Bool -> IO ())
typematicKeyHandlerWrap timeFunc handler = do
  stateVar <- newMVar NoKey
  _ <- forkIO . forever $ do
    sleepTime <- modifyMVar stateVar typematicIteration
    threadDelay . max 0 $ round (1000000 * sleepTime)

  return $ \mchar key isPress -> do
    newValue <-
      if isPress
        then fmap (TypematicRepeat mchar key 0) getCurrentTime
        else return NoKey

    _ <- swapMVar stateVar newValue
    handler mchar key isPress

  where
    typematicIteration state@(TypematicRepeat mchar key count startTime) = do
      now <- getCurrentTime
      let timeDiff = diffUTCTime now startTime
      if timeDiff >= timeFunc count
        then do
          handler mchar key True
          return (TypematicRepeat mchar key (count + 1) startTime,
                  timeFunc (count + 1) - timeDiff)
        else
          return (state, timeFunc count - timeDiff)
    typematicIteration state@NoKey = return (state, timeFunc 0)

