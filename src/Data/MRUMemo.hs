{- Copied & modified from the public-domain uglymemo package by
 - Lennart Augustsson
 -}
module Data.MRUMemo
    ( memoIO, memoIOPure, memo
    ) where

import Control.Concurrent.MVar
import Control.Lens.Operators ((<&>))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Prelude

memoIO :: Eq a => (a -> IO b) -> IO (a -> IO b)
memoIO act =
    memoized <$> newMVar Nothing
    where
        memoized var key = modifyMVar var onMVar
            where
                onMVar j@(Just (oldKey, oldvalue))
                    | oldKey == key = pure (j, oldvalue)
                    | otherwise = callOrig
                onMVar Nothing = callOrig
                callOrig = act key <&> \res -> (Just (key, res), res)

-- | Memoize the given function with a single most-recently-used value
memoIOPure
    :: Eq a
    => (a -> b)           -- ^Function to memoize
    -> IO (a -> IO b)
memoIOPure f =
    newIORef Nothing <&>
    \lastResultRef x ->
    atomicModifyIORef lastResultRef $ \m ->
    let r = f x
        callOrig = (Just (x, r), r)
    in case m of
        Nothing -> callOrig
        Just (key, val)
            | key == x  -> (m, val)
            | otherwise -> callOrig

-- | The pure version of 'memoIO'.
memo
    :: Eq a
    => (a -> b)           -- ^Function to memoize
    -> a -> b
memo f = let f' = unsafePerformIO (memoIOPure f)
         in unsafePerformIO . f'
