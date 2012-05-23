{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Random.Instances()
where

#if __GLASGOW_HASKELL__ < 702

import System.Random (Random(..))
import Control.Arrow (first)
import Data.Word     (Word8)

instance Random Word8 where
  randomR (a,b) = first fromIntegral . randomR (fromIntegral a :: Int, fromIntegral b)
  random        = randomR (minBound, maxBound)

#endif
