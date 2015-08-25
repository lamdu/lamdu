module Control.Concurrent.Utils
    ( forkIOUnmasked, runAfter
    ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, threadDelay)
import Control.Lens.Operators

forkIOUnmasked :: IO () -> IO ThreadId
forkIOUnmasked action = forkIOWithUnmask $ \unmask -> unmask action

runAfter :: Int -> IO () -> IO ThreadId
runAfter delay action =
    do
        threadDelay delay
        action
    & forkIOUnmasked
