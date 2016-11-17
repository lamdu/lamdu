module Control.Concurrent.Utils
    ( forkIOUnmasked, runAfter, asyncThrowTo, forwardExceptions, withForkedIO
    ) where

import           Control.Concurrent (ThreadId, forkIOWithUnmask, threadDelay, myThreadId)
import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (void)

forkIOUnmasked :: IO () -> IO ThreadId
forkIOUnmasked action = forkIOWithUnmask $ \unmask -> unmask action

runAfter :: Int -> IO () -> IO ThreadId
runAfter delay action =
    do
        threadDelay delay
        action
    & forkIOUnmasked

asyncThrowTo :: E.Exception e => ThreadId -> e -> IO ()
asyncThrowTo threadId exc = E.throwTo threadId exc & forkIOUnmasked & void

forwardExceptions :: IO a -> IO (IO a)
forwardExceptions action =
    do
        selfId <- myThreadId
        return $ action `E.catch` \exc@E.SomeException{} ->
            do
                throwerThread <- myThreadId
                show throwerThread ++ " forwarding exception to "
                    ++ show selfId ++ ":"
                    & putStrLn
                print exc
                asyncThrowTo selfId exc
                E.throwIO E.ThreadKilled

withForkedIO :: IO () -> IO a -> IO a
withForkedIO action =
    E.bracket (forkIOUnmasked action) (`E.throwTo` E.ThreadKilled) . const
