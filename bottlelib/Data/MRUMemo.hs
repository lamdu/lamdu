{- Copied & modified from the public-domain uglymemo package by
 - Lennart Augustsson 
 -}
module Data.MRUMemo(memoIO, memo) where

import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- | Memoize the given function with a single most-recently-used value
memoIO :: (Show a, Eq a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    lastResultRef <- newIORef Nothing
    return $ \x -> do
      atomicModifyIORef lastResultRef $ \m ->
        let
          r = f x
          callOrig = (Just (x, r), r)
        in case m of
          Nothing -> callOrig
          Just (key, val)
            | key == x  -> (m, val)
            | otherwise -> callOrig

-- | The pure version of 'memoIO'.
memo :: (Show a, Eq a)
     => (a -> b)           -- ^Function to memoize
     -> (a -> b)
memo f = let f' = unsafePerformIO (memoIO f) in \ x -> unsafePerformIO (f' x)
