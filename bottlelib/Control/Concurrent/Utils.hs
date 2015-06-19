module Control.Concurrent.Utils
    ( forkIOUnmasked
    ) where

import Control.Concurrent (forkIOWithUnmask, ThreadId)

forkIOUnmasked :: IO () -> IO ThreadId
forkIOUnmasked action = forkIOWithUnmask $ \unmask -> unmask action
