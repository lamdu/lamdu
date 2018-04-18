module Control.Concurrent.Extended
    ( module Control.Concurrent
    , forkIOUnmasked, runAfter, forwardSynchronuousExceptions, withForkedIO
    ) where

import           Control.Concurrent
import qualified Control.Exception as E
import qualified Control.Exception.Safe as ES
import           Control.Lens.Operators
import           Control.Monad (void)

import           Prelude

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

forwardSynchronuousExceptions :: IO a -> IO (IO a)
forwardSynchronuousExceptions action =
    myThreadId
    <&>
    \selfId ->
    action `ES.catch`
    \exc@E.SomeException{} ->
    do
        throwerThread <- myThreadId
        show throwerThread ++ " forwarding exception to "
            ++ show selfId ++ ":"
            & putStrLn
        print exc
            `E.catch` \E.SomeException{} ->
                "Failed to resolve exception string" & putStrLn
        asyncThrowTo selfId exc
        E.throwIO E.ThreadKilled

withForkedIO :: IO () -> IO a -> IO a
withForkedIO action =
    E.bracket (forkIOUnmasked action) (`E.throwTo` E.ThreadKilled) . const
