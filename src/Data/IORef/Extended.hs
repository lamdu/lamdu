{-# LANGUAGE TupleSections #-}
module Data.IORef.Extended
    ( module Data.IORef
    , atomicModifyIORef_
    ) where

import Data.IORef
import Prelude

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ var f = atomicModifyIORef var ((, ()) . f)
