{- Copied & modified from the public-domain uglymemo package by
 - Lennart Augustsson 
 -}
module Data.MRUMemo(memoIO, memo) where

import Control.Concurrent.MVar
import System.IO.Unsafe(unsafePerformIO)

-- | Memoize the given function with a single most-recently-used value
memoIO :: (Eq a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    lastResultRef <- newMVar Nothing
    return $ \x -> do
      m <- readMVar lastResultRef
      let
        callOrig = do
          let r = f x
          modifyMVar_ lastResultRef . const . return $ Just (x, r)
          return r
      case m of
        Nothing -> callOrig
        Just (key, val)
          | key == x  -> return val
          | otherwise -> callOrig

-- | The pure version of 'memoIO'.
memo :: (Eq a)
     => (a -> b)           -- ^Function to memoize
     -> (a -> b)
memo f = let f' = unsafePerformIO (memoIO f) in \ x -> unsafePerformIO (f' x)
