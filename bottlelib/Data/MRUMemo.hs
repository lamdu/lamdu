{- Copied & modified from the public-domain uglymemo package by
 - Lennart Augustsson
 -}
module Data.MRUMemo
    ( memoIO, memoIOPure, memo
    ) where

import Control.Concurrent.MVar
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

memoIO :: Eq a => (a -> IO b) -> IO (a -> IO b)
memoIO act =
    do
        var <- newMVar Nothing
        return $ memoized var
    where
        memoized var key = modifyMVar var onMVar
            where
                onMVar j@(Just (oldKey, oldvalue))
                    | oldKey == key = return (j, oldvalue)
                    | otherwise = callOrig
                onMVar Nothing = callOrig
                callOrig =
                    do
                        res <- act key
                        return (Just (key, res), res)

-- | Memoize the given function with a single most-recently-used value
memoIOPure
    :: (Show a, Eq a)
    => (a -> b)           -- ^Function to memoize
    -> IO (a -> IO b)
memoIOPure f =
    do
        lastResultRef <- newIORef Nothing
        return $ \x -> atomicModifyIORef lastResultRef $ \m ->
            let r = f x
                callOrig = (Just (x, r), r)
            in case m of
                Nothing -> callOrig
                Just (key, val)
                    | key == x  -> (m, val)
                    | otherwise -> callOrig

-- | The pure version of 'memoIO'.
memo :: (Show a, Eq a)
          => (a -> b)           -- ^Function to memoize
          -> a -> b
memo f = let f' = unsafePerformIO (memoIOPure f)
         in unsafePerformIO . f'
