{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Random.Instances()
where

#if __GLASGOW_HASKELL__ < 702

import Control.Arrow (first)
import Data.Word     (Word8)
import System.Random (Random(..))

instance Random Word8 where
  randomR (a,b) = first fromIntegral . randomR (fromIntegral a :: Int, fromIntegral b)
  random        = randomR (minBound, maxBound)

#endif
