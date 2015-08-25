{-# LANGUAGE TupleSections #-}
module Data.IORef.Utils
    ( atomicModifyIORef_
    ) where

import Data.IORef

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ var f = atomicModifyIORef var ((, ()) . f)
