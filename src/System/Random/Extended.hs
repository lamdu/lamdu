{-# OPTIONS -fno-warn-orphans #-}
module System.Random.Extended
    ( module System.Random
    , randFunc, genFromHashable
    ) where

import Data.Binary (Binary(..))
import Data.Hashable (Hashable, hashWithSalt)
import System.Random

import Prelude

-- Yucky work-around for lack of Binary instance
instance Binary StdGen where
    get = read <$> get
    put = put . show

randFunc :: (Hashable h, Random r) => h -> r
randFunc = fst . random . genFromHashable

genFromHashable :: Hashable a => a -> StdGen
genFromHashable = mkStdGen . hashWithSalt 0
